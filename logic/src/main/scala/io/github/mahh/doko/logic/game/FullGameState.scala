package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Playing.FinishedTrick
import io.github.mahh.doko.logic.game.FullGameState.TransitionParams
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.score.ScoreAnalyzer
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Reservation.LastingReservation
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap

import scala.reflect.ClassTag

/**
 * Immutable FSM for the game logic.
 *
 * This is the current state of the game (including the history of scores since the start of the game).
 */
sealed trait FullGameState {

  /** Transition to next state. */
  def handleAction: PartialFunction[TransitionParams, FullGameState]

  /** History of scores. */
  def totalScores: TotalScores

  /** The current game states as visible from each player's perspective. */
  def playerStates: TableMap[GameState]

  /** The current game states as visible from a non-playing spectator's perspective. */
  def spectatorState: GameState

}

object FullGameState {

  def initial(implicit rules: Rules): FullGameState = Negotiating.withDealtCards()

  /** Parameters that (possibly) trigger a state transition. */
  type TransitionParams = (PlayerPosition, PlayerAction[GameState])

  /** Shared implementation of all `GameState` implementations (hides type parameters from the base trait). */
  private[game] sealed trait AbstractFullGameState[ClientGS <: GameState, ClientPS] extends FullGameState {

    protected type ClientPlayerState = ClientPS

    protected type ClientGameState = ClientGS

    protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState

    protected def clientPlayerStates: TableMap[ClientPlayerState]

    final def playerStates: TableMap[ClientGameState] = clientPlayerStates.map {
      s => gameState(Some(s))
    }

    final def spectatorState: ClientGS = gameState(None)
  }

  /** Negotiating the reservations. */
  private[game] case class Negotiating private[game](
    starter: PlayerPosition,
    players: TableMap[Negotiating.PlayerState],
    trumps: Trumps.NonSolo,
    totalScores: TotalScores,
    implicit val rules: Rules
  ) extends AbstractFullGameState[GameState, (Seq[Card], Either[Seq[Reservation], Option[Reservation]])] {

   override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      players.map { state =>
        state.hand -> state.reservationState
      }
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): GameState = {
      playerState.fold[GameState] {
        GameState.WaitingForReservations(None)
      } { case (hand, reservationState) =>
        reservationState.fold(
          res => GameState.AskingForReservations(Some(GameState.AskingForReservations.PlayerState(hand, res))),
          res => GameState.WaitingForReservations(Some(GameState.WaitingForReservations.PlayerState(hand, res)))
        )
      }
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.CallReservation(r)) if players(pos).canCall(r) =>
        val updatedPlayers = players + (pos -> players(pos).copy(reservationState = Right(r)))
        if (updatedPlayers.values.forall(_.reservationState.isRight)) {
          NegotiationsResult(starter, updatedPlayers, trumps, totalScores)
        } else {
          copy(players = updatedPlayers)
        }
    }

  }

  private[game] object Negotiating {

    case class PlayerState(
      hand: Seq[Card],
      reservationState: Either[Seq[Reservation], Option[Reservation]]
    ) {
      def canCall(reservation: Option[Reservation]): Boolean = reservationState.left.exists { r =>
        reservation.forall(r.contains)
      }
    }

    def withDealtCards(
      initialPlayer: PlayerPosition = PlayerPosition.Player1,
      totalScores: TotalScores = TotalScores.empty,
    )(
      implicit rules: Rules
    ): Negotiating = {
      import rules._
      withDealtCards(initialPlayer, totalScores, Dealer.dealtCards)
    }

    def withDealtCards(
      initialPlayer: PlayerPosition,
      totalScores: TotalScores,
      allCards: TableMap[Seq[Card]],
    )(
      implicit rules: Rules
    ): Negotiating = {

      val trumps: Trumps.NonSolo = {
        val isPiglets = allCards.values.exists(_.count(_ == Fox) > 1)
        if (isPiglets) Trumps.Piglets else Trumps.Default
      }

      val states: TableMap[Negotiating.PlayerState] =
        allCards.map { cards =>
          val sortedCards = cards.sorted(trumps.cardsOrdering)
          val reservations = Reservations.possibleReservations(sortedCards)
          PlayerState(sortedCards, Left(reservations))
        }
      Negotiating(initialPlayer, states, trumps, totalScores, rules)
    }
  }

  /** Reservations have been made, result is presented. */
  private[game] case class NegotiationsResult(
    starter: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    result: Option[(PlayerPosition, Reservation)],
    missingAcks: Set[PlayerPosition],
    trumps: Trumps,
    totalScores: TotalScores,
    implicit val rules: Rules
  ) extends AbstractFullGameState[GameState.ReservationResult, GameState.ReservationResult.PlayerState] {

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      players.map { state =>
        GameState.ReservationResult.PlayerState(state.hand)
      }
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.ReservationResult(result, playerState)
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.AcknowledgeReservation) =>
        val stillMissing = missingAcks - pos
        if (stillMissing.isEmpty) {
          result.fold[FullGameState] {
            Playing(starter, players, None, trumps, totalScores)
          } {
            case (p, l: LastingReservation) =>
              Playing(starter, players, Some(p -> l), trumps, totalScores)
            case (_, Reservation.Throwing) =>
              Negotiating.withDealtCards(starter, totalScores)
            case (p, Reservation.Poverty) =>
              PovertyOnOffer(starter, p, players, trumps, PlayerPosition.next(p), totalScores, rules)
          }

        } else {
          copy(missingAcks = stillMissing)
        }
    }

  }

  private[game] object NegotiationsResult {

    def apply(
      starter: PlayerPosition,
      players: TableMap[Negotiating.PlayerState],
      trumps: Trumps.NonSolo,
      totalScores: TotalScores
    )(
      implicit rules: Rules
    ): NegotiationsResult = {
      def collectReservation[R <: Reservation : ClassTag]: Option[(PlayerPosition, Reservation)] = {
        PlayerPosition.trickOrder(starter)
          .flatMap { p =>
            players(p).reservationState.toOption.flatten.collect {
              case r: R => p -> r
            }
          }.headOption
      }

      val winningReservation: Option[(PlayerPosition, Reservation)] =
        collectReservation[Reservation.Solo] orElse
          collectReservation[Reservation.Throwing.type] orElse
          collectReservation[Reservation.Poverty.type] orElse
          collectReservation[Reservation.Marriage.type]

      val newTrumps = winningReservation match {
        case Some(_ -> Reservation.Solo(s)) => s
        case _ => trumps
      }

      val updatedPlayers: TableMap[Playing.PlayerState] = players.mapWithPos { (pos, state) =>
        val hand = if (newTrumps == trumps) state.hand else state.hand.sorted(newTrumps.cardsOrdering)
        val role: Role = winningReservation match {
          case Some((p, s: Reservation.Solo)) =>
            if (p == pos) Role.Solo(s) else Role.Kontra
          case Some((p, Reservation.Marriage)) =>
            if (p == pos) Role.Marriage else Role.Kontra
          case Some((p, Reservation.Poverty)) =>
            if (p == pos) Role.Re else Role.Kontra
          case _ =>
            val queenOfClubsCount = hand.count(_ == QueenOfClubs)
            if (queenOfClubsCount > 1) {
              Role.SilentMarriage
            } else if (queenOfClubsCount == 1) {
              Role.Re
            } else {
              Role.Kontra
            }
        }
        Playing.PlayerState(hand, role, None)
      }
      NegotiationsResult(
        starter,
        updatedPlayers,
        winningReservation,
        PlayerPosition.All.toSet,
        newTrumps,
        totalScores,
        rules
      )
    }
  }

  /** A "poverty" is being offered to the other players. */
  private[game] case class PovertyOnOffer(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    trumps: Trumps,
    playerBeingOffered: PlayerPosition,
    totalScores: TotalScores,
    implicit val rules: Rules
  ) extends AbstractFullGameState[GameState.PovertyOnOffer, GameState.PovertyOnOffer.PlayerState] {

    private val onOffer = players(poorPlayer).hand.count(trumps.isTrump)

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      players.mapWithPos { (p, s) =>
        GameState.PovertyOnOffer.PlayerState(s.hand, playerBeingOffered == p)
      }
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.PovertyOnOffer(onOffer, poorPlayer, playerState)
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.PovertyReply(accepted)) if pos == playerBeingOffered =>
        if (accepted) {
          PovertyExchange(starter, poorPlayer, pos, players, trumps, totalScores, rules)
        } else {
          val nextPlayer = PlayerPosition.next(pos)
          if (nextPlayer == poorPlayer) {
            PovertyRefused(starter, poorPlayer, totalScores, rules)
          } else {
            copy(playerBeingOffered = nextPlayer)
          }
        }
    }

  }

  /** The "poverty" has been refused (which is presented to the players before re-dealing). */
  private[game] case class PovertyRefused(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    totalScores: TotalScores,
    implicit val rules: Rules,
    missingAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
  ) extends AbstractFullGameState[GameState.PovertyRefused, GameState.PovertyRefused.PlayerState.type] {

    override protected type ClientPlayerState = GameState.PovertyRefused.PlayerState.type

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      TableMap.fill(GameState.PovertyRefused.PlayerState)
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.PovertyRefused(playerState)
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.AcknowledgePovertyRefused) =>
        val stillPending = missingAcks - pos
        if (stillPending.isEmpty) {
          Negotiating.withDealtCards(starter, totalScores)
        } else {
          copy(missingAcks = stillPending)
        }
    }

  }

  /** A player has accepted the poverty and now needs to select the cards that shall be returned. */
  private[game] case class PovertyExchange(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    acceptingPlayer: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    trumps: Trumps,
    totalScores: TotalScores,
    implicit val rules: Rules
  ) extends AbstractFullGameState[GameState.PovertyExchange, GameState.PovertyExchange.PlayerState] {
    private implicit val cardsOrdering: Ordering[Card] = trumps.cardsOrdering

    private val (onOffer, poorPlayersCards) = players(poorPlayer).hand.partition(trumps.isTrump)
    private val choices = (players(acceptingPlayer).hand ++ onOffer).sorted

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      players.mapWithPos { (p, s) =>
        val (hand, role) = p match {
          case `poorPlayer` =>
            poorPlayersCards -> GameState.PovertyExchange.Poor
          case `acceptingPlayer` =>
            choices -> GameState.PovertyExchange.Accepting
          case _ =>
            s.hand -> GameState.PovertyExchange.NotInvolved
        }
        GameState.PovertyExchange.PlayerState(hand, role)
      }
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.PovertyExchange(onOffer.size, poorPlayer, acceptingPlayer, playerState)
    }

    private def isAllowedReturn(cards: Seq[Card]): Boolean = {
      (choices diff cards).size == rules.deckRule.cardsPerPlayer
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (`acceptingPlayer`, PlayerAction.PovertyReturned(cards)) if isAllowedReturn(cards) =>
        val updatedPlayers: TableMap[Playing.PlayerState] = players.mapWithPos {
          case (`acceptingPlayer`, state) =>
            state.copy(hand = choices diff cards, role = Role.Re)
          case (`poorPlayer`, state) =>
            state.copy(hand = (poorPlayersCards ++ cards).sorted)
          case (_, state) =>
            state
        }
        // TODO: if we want to display things like how many trumps were returned,
        //  this would be the place to do it:
        Playing(starter, updatedPlayers, None, trumps, totalScores)
    }

  }

  /** The actual (round of the) game is being played. */
  private[game] case class Playing(
    starter: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    reservation: Option[(PlayerPosition, LastingReservation)],
    trumps: Trumps,
    currentTrick: Trick,
    totalScores: TotalScores,
    implicit val rules: Rules,
    wonTricks: List[(PlayerPosition, CompleteTrick)] = List.empty,
    finishedTrickOpt: Option[FinishedTrick] = None,
  ) extends AbstractFullGameState[GameState.Playing, GameState.Playing.PlayerState] {

    import Playing._

    private val playableCards: TableMap[Set[Card]] =
      TrickAnalyzer.playableCards(players.map(_.hand), currentTrick, trumps)

    private val possibleBids: Map[PlayerPosition, Bid] =
      BidAnalyzer.nextPossibleBids(
        currentTrick,
        wonTricks,
        players.map(_.role),
        players.toMap.flatMap { case (k, v) => v.bid.map(k -> _) }
      )

    private val bids: Map[PlayerPosition, NameableBid] =
      players.toMap.flatMap { case (pos, state) => state.bid.map(pos -> NameableBid(Role.isElders(state.role), _)) }
    private val trickCounts: Map[PlayerPosition, Int] =
      wonTricks.groupBy { case (k, _) => k }.map { case (k, v) => k -> v.size }

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      players.mapWithPos { (pos, state) =>
        GameState.Playing.PlayerState(
          state.hand,
          possibleBid = possibleBids.get(pos).map(NameableBid(Role.isElders(state.role), _)),
          playableCards(pos),
          finishedTrickOpt.exists(_.missingAcks.contains(pos))
        )
      }
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.Playing(currentTrick, bids, reservation, trickCounts, playerState, finishedTrickOpt.map(_.winner))
    }

    private def isMarriageRound: Boolean = wonTricks.sizeCompare(MarriageRounds) < 0

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.PlayCard(c)) if playableCards(pos).contains(c) =>
        val updatedPlayers = players.modified(pos)(_.withoutCard(c))
        val updatedTrick = currentTrick.copy(cards = currentTrick.cards + (pos -> c))

        updatedTrick.asCompleteTrick.fold {
          copy(players = updatedPlayers, currentTrick = updatedTrick)
        } { completeTrick =>
          val winner = TrickAnalyzer.winner(completeTrick, trumps)
          val finishedTrick = FinishedTrick(winner, completeTrick)
          copy(players = updatedPlayers, currentTrick = updatedTrick, finishedTrickOpt = Some(finishedTrick))
        }

      case (pos, PlayerAction.AcknowledgeTrickResult) if finishedTrickOpt.nonEmpty =>
        val finished = finishedTrickOpt.get
        val stillMissing = finished.missingAcks - pos
        if (stillMissing.nonEmpty) {
          copy(finishedTrickOpt = Some(finished.copy(missingAcks = stillMissing)))
        } else {
          val updatedWonTricks = (finished.winner -> finished.trick) :: wonTricks
          val updatedPlayers = {
            def alreadyMarried: Boolean = players.values.exists(_.role == Role.Married)

            reservation match {
              case Some(_ -> Reservation.Marriage) if alreadyMarried =>
                players
              case Some(p -> Reservation.Marriage) if finished.winner != p && isMarriageRound =>
                players.modified(finished.winner)(_.copy(role = Role.Married))
              case Some(p -> Reservation.Marriage) if !isMarriageRound =>
                players.modified(p)(_.copy(role = Role.MarriageSolo))
              case _ =>
                players
            }
          }
          if (updatedPlayers.values.forall(_.hand.isEmpty)) {
            RoundResults(
              starter,
              updatedPlayers,
              updatedWonTricks,
              totalScores
            )
          } else {
            copy(
              players = updatedPlayers,
              currentTrick = Trick(finished.winner, Map.empty),
              finishedTrickOpt = None,
              wonTricks = updatedWonTricks
            )
          }

        }
      case (pos, PlayerAction.PlaceBid(b)) if possibleBids.get(pos).exists(Bid.ordering.lteq(_, b)) =>
        val updatedPlayers = players.modified(pos)(_.copy(bid = Some(b)))
        copy(players = updatedPlayers)
    }

  }

  private[game] object Playing {

    def apply(
      starter: PlayerPosition,
      players: TableMap[Playing.PlayerState],
      reservation: Option[(PlayerPosition, LastingReservation)],
      trumps: Trumps,
      totalScores: TotalScores
    )(
      implicit rules: Rules
    ): Playing = {
      Playing(starter, players, reservation, trumps, Trick(starter, Map.empty), totalScores, rules)
    }

    private implicit class RichPlayersMap(private val players: TableMap[PlayerState]) extends AnyVal {
      def modified(pos: PlayerPosition)(f: PlayerState => PlayerState): TableMap[PlayerState] = {
        players + (pos -> f(players(pos)))
      }
    }

    case class FinishedTrick(
      winner: PlayerPosition,
      trick: CompleteTrick,
      missingAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
    )

    case class PlayerState(
      hand: Seq[Card],
      role: Role,
      bid: Option[Bid]
    ) {
      def withoutCard(card: Card): PlayerState = {
        val pos = hand.indexOf(card)
        val remaining = hand.take(pos) ++ hand.drop(pos + 1)
        copy(hand = remaining)
      }
    }

  }

  /** Round is finished, results are presented. */
  private[game] case class RoundResults(
    starter: PlayerPosition,
    scores: Scores,
    totalScores: TotalScores,
    implicit val rules: Rules,
    missingAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
  ) extends AbstractFullGameState[GameState.RoundResults, GameState.RoundResults.PlayerState.type] {

    override protected val clientPlayerStates: TableMap[ClientPlayerState] = {
      TableMap.fill(GameState.RoundResults.PlayerState)
    }

    override protected def gameState(playerState: Option[ClientPlayerState]): ClientGameState = {
      GameState.RoundResults(scores, playerState)
    }

    override def handleAction: PartialFunction[TransitionParams, FullGameState] = {
      case (pos, PlayerAction.AcknowledgeRoundResult) =>
        val stillMissing = missingAcks - pos
        if (stillMissing.isEmpty) {
          Negotiating.withDealtCards(PlayerPosition.next(starter), totalScores)
        } else {
          copy(missingAcks = stillMissing)
        }
    }

  }

  private[game] object RoundResults {
    def apply(
      starter: PlayerPosition,
      players: TableMap[Playing.PlayerState],
      wonTricks: List[(PlayerPosition, CompleteTrick)],
      totalScores: TotalScores
    )(
      implicit rules: Rules
    ): RoundResults = {

      val scores = ScoreAnalyzer.scores(
        players.toMap.flatMap { case (p, s) => s.bid.map(p -> _) },
        wonTricks,
        players.map(_.role)
      )

      RoundResults(starter, scores, totalScores.addScores(scores), rules)
    }
  }

}

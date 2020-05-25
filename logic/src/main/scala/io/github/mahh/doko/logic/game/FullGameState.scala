package io.github.mahh.doko.logic.game

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
  def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState]

  /** The current (partial) game states as visible from each player's perspective. */
  def playerStates: Map[PlayerPosition, GameState]

  /** History of scores. */
  def totalScores: TotalScores

}

object FullGameState {

  val initial: FullGameState = Joining(Set.empty)


  /** Players are joining / not all four players have joined yet. */
  // TODO: move this somewhere else. The game logic is complex enough, the setup phase should be separated
  private[game] case class Joining(
    players: Set[PlayerPosition] = Set.empty
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.Join) if !players.contains(pos) =>
        val updatedPlayers = players + pos
        val complete = PlayerPosition.AllAsSet == updatedPlayers
        if (complete) {
          Negotiating.withDealtCards(totalScores = totalScores)
        } else {
          Joining(updatedPlayers)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      players.map(_ -> GameState.Joining).toMap
    }


    override def totalScores: TotalScores = TotalScores(Nil)
  }

  /** Negotiating the reservations. */
  private[game] case class Negotiating(
    starter: PlayerPosition,
    players: TableMap[Negotiating.PlayerState],
    trumps: Trumps.NonSolo,
    totalScores: TotalScores
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.CallReservation(r)) if players(pos).canCall(r) =>
        val updatedPlayers = players + (pos -> players(pos).copy(reservationState = Right(r)))
        if (updatedPlayers.values.forall(_.reservationState.isRight)) {
          NegotiationsResult(starter, updatedPlayers, trumps, totalScores)
        } else {
          copy(players = updatedPlayers)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      players.map { state =>
        val gameState = state.reservationState.fold(
          GameState.AskingForReservations(state.hand, _),
          GameState.WaitingForReservations(state.hand, _)
          )
        gameState
      }.toMap
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
      totalScores: TotalScores,
      allCards: TableMap[List[Card]] = dealtCards,
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
      Negotiating(initialPlayer, states, trumps, totalScores)
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
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.AcknowledgeReservation) =>
        val stillMissing = missingAcks - pos
        if (stillMissing.isEmpty) {
          result.fold[FullGameState] {
            Playing(starter, players, None, trumps, Trick(starter, Map.empty), totalScores)
          } {
            case (p, l: LastingReservation) =>
              Playing(starter, players, Some(p -> l), trumps, Trick(starter, Map.empty), totalScores)
            case (_, Reservation.Throwing) =>
              Negotiating.withDealtCards(starter, totalScores)
            case (p, Reservation.Poverty) =>
              PovertyOnOffer(starter, p, players, trumps, PlayerPosition.next(p), totalScores)
          }

        } else {
          copy(missingAcks = stillMissing)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      players.map { state =>
        GameState.ReservationResult(state.hand, result)
      }.toMap
    }

  }

  private[game] object NegotiationsResult {

    def apply(
      starter: PlayerPosition,
      players: TableMap[Negotiating.PlayerState],
      trumps: Trumps.NonSolo,
      totalScores: TotalScores
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
            if (p == pos) Role.Poverty else Role.Kontra
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
      NegotiationsResult(starter, updatedPlayers, winningReservation, PlayerPosition.All.toSet, newTrumps, totalScores)
    }
  }

  /** A "poverty" is being offered to the other players. */
  private[game] case class PovertyOnOffer(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    trumps: Trumps,
    playerBeingOffered: PlayerPosition,
    totalScores: TotalScores
  ) extends FullGameState {

    private val onOffer = players(poorPlayer).hand.count(trumps.isTrump)

    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.PovertyReply(accepted)) if pos == playerBeingOffered =>
        if (accepted) {
          // TODO poverty accepted
          PovertyRefused(starter, poorPlayer, totalScores)
        } else {
          val nextPlayer = PlayerPosition.next(pos)
          if (nextPlayer == playerBeingOffered) {
            PovertyRefused(starter, poorPlayer, totalScores)
          } else {
            copy(playerBeingOffered = nextPlayer)
          }
        }
    }

    override def playerStates: Map[PlayerPosition, GameState] = players.mapWithPos { (p, s) =>
      GameState.PovertyOnOffer(s.hand, onOffer, poorPlayer, playerBeingOffered == p)
    }.toMap

  }

  /** The "poverty" has been refused (which is presented to the players before re-dealing). */
  private[game] case class PovertyRefused(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    totalScores: TotalScores,
    missingAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.AcknowledgePovertyRefused) =>
        val stillPending = missingAcks - pos
        if (stillPending.isEmpty) {
          Negotiating.withDealtCards(starter, totalScores)
        } else {
          copy(missingAcks = stillPending)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] = PlayerPosition.All.map(_ -> GameState.PovertyRefused).toMap
  }

  /** The actual (round of the) game is being played. */
  private[game] case class Playing(
    starter: PlayerPosition,
    players: TableMap[Playing.PlayerState],
    reservation: Option[(PlayerPosition, LastingReservation)],
    trumps: Trumps,
    currentTrick: Trick,
    totalScores: TotalScores,
    wonTricks: List[(PlayerPosition, CompleteTrick)] = List.empty,
    finishedTrick: Option[(PlayerPosition, CompleteTrick)] = None,
    pendingTrickAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
  ) extends FullGameState {

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

    private def isMarriageRound: Boolean = wonTricks.sizeCompare(MarriageRounds) < 0

    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.PlayCard(c)) if playableCards(pos).contains(c) =>
        val updatedPlayers = players.modified(pos)(_.withoutCard(c))
        val updatedTrick = currentTrick.copy(cards = currentTrick.cards + (pos -> c))

        updatedTrick.asCompleteTrick.fold {
          copy(players = updatedPlayers, currentTrick = updatedTrick)
        } { completeTrick =>
          val winner = TrickAnalyzer.winner(completeTrick, trumps)
          copy(players = updatedPlayers, currentTrick = updatedTrick, finishedTrick = Some(winner -> completeTrick))
        }

      case (pos, PlayerAction.AcknowledgeTrickResult) if finishedTrick.nonEmpty =>
        val stillPending = pendingTrickAcks - pos
        if (stillPending.nonEmpty) {
          copy(pendingTrickAcks = stillPending)
        } else {
          val (winner, completeTrick) = finishedTrick.get
          val updatedPlayers = {
            def alreadyMarried: Boolean = players.values.forall(_.role != Role.Married)

            reservation match {
              case Some(_ -> Reservation.Marriage) if alreadyMarried =>
                players
              case Some(p -> Reservation.Marriage) if winner != p && isMarriageRound =>
                players.modified(winner)(_.copy(role = Role.Married))
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
              wonTricks,
              totalScores
            )
          } else {
            copy(
              players = updatedPlayers,
              currentTrick = Trick(winner, Map.empty),
              finishedTrick = None,
              pendingTrickAcks = PlayerPosition.AllAsSet,
              wonTricks = (winner -> completeTrick) :: wonTricks
            )
          }

        }
      case (pos, PlayerAction.PlaceBid(b)) if possibleBids.get(pos).exists(Bid.ordering.lteq(_, b)) =>
        val updatedPlayers = players.modified(pos)(_.copy(bid = Some(b)))
        copy(players = updatedPlayers)
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      val bids: Map[PlayerPosition, NameableBid] =
        players.toMap.flatMap { case (pos, state) => state.bid.map(pos -> NameableBid(Role.isElders(state.role), _)) }
      val trickCounts: Map[PlayerPosition, Int] =
        wonTricks.groupBy { case (k, _) => k }.map { case (k, v) => k -> v.size }
      players.mapWithPos { (pos, state) =>
        GameState.Playing(
          state.hand,
          currentTrick,
          bids,
          reservation,
          possibleBid = possibleBids.get(pos).map(NameableBid(Role.isElders(state.role), _)),
          trickCounts,
          playableCards(pos),
          finishedTrick.map { case (w, _) =>
            w -> pendingTrickAcks.contains(pos)
          }
        )
      }.toMap
    }

  }

  private[game] object Playing {

    private implicit class RichPlayersMap(private val players: TableMap[PlayerState]) extends AnyVal {
      def modified(pos: PlayerPosition)(f: PlayerState => PlayerState): TableMap[PlayerState] = {
        players + (pos -> f(players(pos)))
      }
    }

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
    missingAcks: Set[PlayerPosition] = PlayerPosition.AllAsSet
  ) extends FullGameState {


    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.AcknowledgeRoundResult) =>
        val stillMissing = missingAcks - pos
        if (stillMissing.isEmpty) {
          Negotiating.withDealtCards(PlayerPosition.next(starter), totalScores)
        } else {
          copy(missingAcks = stillMissing)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] =
      PlayerPosition.All.map(_ -> GameState.RoundResults(scores)).toMap

  }

  private[game] object RoundResults {
    def apply(
      starter: PlayerPosition,
      players: TableMap[Playing.PlayerState],
      wonTricks: List[(PlayerPosition, CompleteTrick)],
      totalScores: TotalScores
    ): RoundResults = {

      val scores = ScoreAnalyzer.scores(
        players.toMap.flatMap { case (p, s) => s.bid.map(p -> _) },
        wonTricks,
        players.map(_.role)
      )

      RoundResults(starter, scores, totalScores.addScores(scores))
    }
  }

}

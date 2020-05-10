package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.score.ScoreAnalyzer
import io.github.mahh.doko.shared.bids.TotalBid
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Reservation.LastingReservation
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.TotalScores

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


  // TODO: move this somewhere else. The game logic is complex enough, the setup phase should be separated
  private[logic] case class Joining(
    players: Set[PlayerPosition] = Set.empty
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.Join) if !players.contains(pos) =>
        val updatedPlayers = players + pos
        val complete = PlayerPosition.All.toSet == updatedPlayers
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

  private[logic] case class Negotiating(
    starter: PlayerPosition,
    players: Map[PlayerPosition, Negotiating.PlayerState],
    trumps: Trumps.NonSolo,
    totalScores: TotalScores
  ) extends FullGameState {
    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.CallReservation(r)) if players.get(pos).exists(_.canCall(r)) =>
        val updatedPlayers = players + (pos -> players(pos).copy(reservationState = Right(r)))
        if (updatedPlayers.values.forall(_.reservationState.isRight)) {
          NegotiationsResult(starter, updatedPlayers, trumps, totalScores)
        } else {
          copy(players = updatedPlayers)
        }
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      players.map { case (pos, state) =>
        val gameState = state.reservationState.fold(
          GameState.AskingForReservations(state.hand, _),
          GameState.WaitingForReservations(state.hand, _)
        )
        pos -> gameState
      }
    }

  }

  private[logic] object Negotiating {

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
      allCards: Map[PlayerPosition, List[Card]] = dealtCards,
    ): Negotiating = {

      val trumps: Trumps.NonSolo = {
        val isPiglets = allCards.values.exists(_.count(_ == Fox) > 1)
        if (isPiglets) Trumps.Piglets else Trumps.Default
      }

      val states: Map[PlayerPosition, PlayerState] =
        allCards.map { case (pos, cards) =>
          val sortedCards = cards.sorted(trumps.cardsOrdering)
          val reservations = Reservations.possibleReservations(sortedCards)
          pos -> PlayerState(sortedCards, Left(reservations))
        }
      Negotiating(initialPlayer, states, trumps, totalScores)
    }
  }

  private[logic] case class NegotiationsResult(
    starter: PlayerPosition,
    players: Map[PlayerPosition, Playing.PlayerState],
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
      players.map { case (pos, state) =>
        pos -> GameState.ReservationResult(state.hand, result)
      }
    }

  }

  private[logic] object NegotiationsResult {

    def apply(
      starter: PlayerPosition,
      players: Map[PlayerPosition, Negotiating.PlayerState],
      trumps: Trumps.NonSolo,
      totalScores: TotalScores
    ): NegotiationsResult = {
      def collectReservation[R <: Reservation : ClassTag]: Option[(PlayerPosition, Reservation)] = {
        PlayerPosition.trickOrder(starter)
          .flatMap { p =>
            players.get(p).flatMap(_.reservationState.toOption.flatten.collect {
              case r: R => p -> r
            })
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

      val updatedPlayers: Map[PlayerPosition, Playing.PlayerState] = players.map { case (pos, state) =>
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
        pos -> Playing.PlayerState(hand, role, None)
      }
      NegotiationsResult(starter, updatedPlayers, winningReservation, PlayerPosition.All.toSet, newTrumps, totalScores)
    }
  }

  private[logic] case class PovertyOnOffer(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    players: Map[PlayerPosition, Playing.PlayerState],
    trumps: Trumps,
    playerBeingOffered: PlayerPosition,
    totalScores: TotalScores
  ) extends FullGameState {

    private val onOffer = players.get(poorPlayer).map(_.hand.count(trumps.isTrump)).getOrElse(0)

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

    override def playerStates: Map[PlayerPosition, GameState] = players.map { case (p, s) =>
      p -> GameState.PovertyOnOffer(s.hand, onOffer, poorPlayer, playerBeingOffered == p)
    }

  }

  private[logic] case class PovertyRefused(
    starter: PlayerPosition,
    poorPlayer: PlayerPosition,
    totalScores: TotalScores,
    missingAcks: Set[PlayerPosition] = PlayerPosition.All.toSet
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

  private[logic] case class Playing(
    starter: PlayerPosition,
    players: Map[PlayerPosition, Playing.PlayerState],
    reservation: Option[(PlayerPosition, LastingReservation)],
    trumps: Trumps,
    currentTrick: Trick,
    totalScores: TotalScores,
    wonTricks: List[(PlayerPosition, Trick)] = List.empty,
    trickWinner: Option[PlayerPosition] = None,
    pendingTrickAcks: Set[PlayerPosition] = PlayerPosition.All.toSet
  ) extends FullGameState {

    import Playing._

    private val playableCards =
      TrickLogic.playableCards(players.map { case (k, v) => k -> v.hand }, currentTrick, trumps)

    private def numberOfPlayedTricks: Int = wonTricks.size

    override def handleAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullGameState] = {
      case (pos, PlayerAction.PlayCard(c)) if playableCards.get(pos).exists(_.contains(c)) =>
        val updatedPlayers = players.modified(pos)(_.withoutCard(c))
        val updatedTrick = currentTrick.copy(cards = currentTrick.cards + (pos -> c))

        if (updatedTrick.isComplete) {
          copy(players = updatedPlayers, currentTrick = updatedTrick, trickWinner = TrickLogic.winner(updatedTrick, trumps))
        } else {
          copy(players = updatedPlayers, currentTrick = updatedTrick)
        }
      case (pos, PlayerAction.AcknowledgeTrickResult) if trickWinner.nonEmpty =>
        val stillPending = pendingTrickAcks - pos
        if (stillPending.nonEmpty) {
          copy(pendingTrickAcks = stillPending)
        } else {
          val winner = trickWinner.get
          val updatedPlayers = {
            val isWedding = reservation.exists { case (p, r) =>
              r == Reservation.Marriage && winner != p && numberOfPlayedTricks < 3
            }
            if (isWedding) players.modified(winner)(_.copy(role = Role.Married)) else players
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
              currentTrick = Trick(trickWinner.get, Map.empty),
              trickWinner = None,
              pendingTrickAcks = PlayerPosition.All.toSet,
              wonTricks = (winner -> currentTrick) :: wonTricks
            )
          }

        }
      // TODO: calls
    }

    override val playerStates: Map[PlayerPosition, GameState] = {
      val bids: Map[PlayerPosition, TotalBid] =
        players.flatMap { case (pos, state) => state.bids.map(pos -> _) }
      val trickCounts: Map[PlayerPosition, Int] =
        wonTricks.groupBy { case (k, _) => k }.map { case (k, v) => k -> v.size }
      players.map { case (pos, state) =>
        pos -> GameState.Playing(
          state.hand,
          currentTrick,
          bids,
          reservation,
          // TODO:
          possibleBid = None,
          trickCounts,
          playableCards.getOrElse(pos, Set.empty),
          trickWinner.map(w => w -> pendingTrickAcks.contains(pos))
        )
      }
    }

  }

  object Playing {

    private implicit class RichPlayersMap(private val players: Map[PlayerPosition, PlayerState]) extends AnyVal {
      def modified(pos: PlayerPosition)(f: PlayerState => PlayerState): Map[PlayerPosition, PlayerState] = {
        players.get(pos).map(f).fold(players)(p => players + (pos -> p))
      }
    }

    case class PlayerState(
      hand: Seq[Card],
      role: Role,
      bids: Option[TotalBid]
      // TODO Calls
    ) {
      def withoutCard(card: Card): PlayerState = {
        val pos = hand.indexOf(card)
        val remaining = hand.take(pos) ++ hand.drop(pos + 1)
        copy(hand = remaining)
      }
    }

  }

  case class RoundResults(
    starter: PlayerPosition,
    scores: Scores,
    totalScores: TotalScores,
    missingAcks: Set[PlayerPosition] = PlayerPosition.All.toSet
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

  object RoundResults {
    def apply(
      starter: PlayerPosition,
      players: Map[PlayerPosition, Playing.PlayerState],
      wonTricks: List[(PlayerPosition, Trick)],
      totalScores: TotalScores
    ): RoundResults = {

      val scores = ScoreAnalyzer.scores(
        players.flatMap { case (p, s) => s.bids.map(p -> _) },
        wonTricks,
        players.map { case (p, s) => p -> s.role }
      )

      RoundResults(starter, scores, totalScores.addScores(scores))
    }
  }

}

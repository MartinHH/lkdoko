package io.github.mahh.doko.client.state

import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import io.github.mahh.doko.client.strings.AnnouncementStrings
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.Playing
import io.github.mahh.doko.shared.game.GameState.PovertyOnOffer
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap

/**
 * Models the client's state using (private) Vars and (public) Signals derived from those Vars.
 */
class Signals {

  private val stateVar = Var(ClientGameState.Connecting)

  private val playerNamesVar = Var(Map.empty[PlayerPosition, String])

  private val totalScoresVar = Var(Map.empty[PlayerPosition, Int])

  def updateClientGameState(gameState: ClientGameState): Unit =
    stateVar.set(gameState)

  def updatePlayerNames(names: Map[PlayerPosition, String]): Unit =
    playerNamesVar.set(names)

  def updateTotalScores(totalScores: TotalScores): Unit =
    totalScoresVar.set(totalScores.sumPerPlayer)

  private def collectGameState[A](pf: PartialFunction[GameState, A]): Signal[Option[A]] =
    stateVar.toObservable.map {
      case ClientGameState.GameInProgress(pf(a)) =>
        Some(a)
      case _ =>
        None
    }

  private def collectGameStateOrElse[A](default: => A)(
    pf: PartialFunction[GameState, A]
  ): Signal[A] =
    stateVar.toObservable.map {
      case ClientGameState.GameInProgress(pf(a)) =>
        a
      case _ =>
        default
    }

  val playerMarker: Signal[Option[PlayerMarker]] = collectGameState { case p: Playing =>
    p.trickWinner.map(PlayerMarker.TrickWinner.apply) orElse
      p.currentTrick.currentPlayer.map(PlayerMarker.Next.apply)
  }.map(_.flatten)

  val bids: Signal[Map[PlayerPosition, NameableBid]] = collectGameStateOrElse(Map.empty) {
    case p: GameState.Playing => p.bids
  }

  val trickCounts: Signal[Map[PlayerPosition, Int]] = collectGameStateOrElse(Map.empty) {
    case p: GameState.Playing => p.trickCounts
  }

  val possibleReservations: Signal[Option[Set[Reservation]]] = collectGameState {
    case a: GameState.AskingForReservations => a.possibleReservations.toSet
  }

  val bidsConfig: Signal[BidsConfig] =
    collectGameStateOrElse(BidsConfig()) { case p: GameState.Playing =>
      p.playerState.fold(BidsConfig()) { ps =>
        BidsConfig(ps.isElders, ps.possibleBid)
      }
    }

  val povertyOffered: Signal[Boolean] = collectGameStateOrElse(false) { case p: PovertyOnOffer =>
    p.playerIsBeingAsked
  }

  val results: Signal[Option[RoundResults]] = collectGameState { case rr: RoundResults => rr }

  val hand: Signal[Seq[CardConfig]] = stateVar.toObservable.map {
    case ClientGameState.GameInProgress(gs) =>
      CardConfig.handForState(gs)
    case _ =>
      Seq.empty
  }

  val trick: Signal[Map[PlayerPosition, CardConfig]] = stateVar.toObservable.map {
    case ClientGameState.GameInProgress(gs) =>
      CardConfig.trickForState(gs)
    case _ =>
      Map.empty
  }

  val ackConfig: Signal[Option[AckConfig]] = collectGameStateOrElse(None) { case gs =>
    AckConfig.forState(gs)
  }

  val playerNames: Signal[TableMap[String]] =
    playerNamesVar.toObservable.map(TableMap.fromMapOrElse(_, _.toString))

  val totalScores: Signal[Map[PlayerPosition, Int]] = totalScoresVar.toObservable

  val announcementString: Signal[String] =
    stateVar.toObservable.combineWithFn(playerNames)(AnnouncementStrings.forConnectionState)

  val nameInputHidden: Signal[Boolean] = stateVar.toObservable.map {
    case ClientGameState.GameInProgress(s) =>
      s.playerState.nonEmpty
    case ClientGameState.Joining =>
      true
    case _ =>
      false
  }
}

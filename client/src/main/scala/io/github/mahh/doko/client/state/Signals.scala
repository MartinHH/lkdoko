package io.github.mahh.doko.client.state

import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
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
// TODO: ideally, this would model all of the client's state, accepting updates from the server
//   and propagating them via the Signals. However: there's still a lot of refactoring ahead...
class Signals {

  private val gameStateVar = Var(Option.empty[GameState])

  private val playerNamesVar = Var(Map.empty[PlayerPosition, String])

  private val totalScoresVar = Var(Map.empty[PlayerPosition, Int])

  def updateGameState(gameState: GameState): Unit =
    gameStateVar.set(Some(gameState))

  def updatePlayerNames(names: Map[PlayerPosition, String]): Unit =
    playerNamesVar.set(names)

  def updateTotalScores(totalScores: TotalScores): Unit =
    totalScoresVar.set(totalScores.sumPerPlayer)

  private def collectGameState[A](pf: PartialFunction[GameState, A]): Signal[Option[A]] =
    gameStateVar.toObservable.map(_.collect(pf))

  private def collectGameStateOrElse[A](default: => A)(
    pf: PartialFunction[GameState, A]
  ): Signal[A] =
    gameStateVar.toObservable.map(_.collect(pf).getOrElse(default))

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

  val hand: Signal[Seq[CardConfig]] = gameStateVar.toObservable.map { stateOpt =>
    stateOpt.fold(Seq.empty)(CardConfig.handForState)
  }

  val trick: Signal[Map[PlayerPosition, CardConfig]] = gameStateVar.toObservable.map { stateOpt =>
    stateOpt.fold(Map.empty)(CardConfig.trickForState)
  }

  val ackConfig: Signal[Option[AckConfig]] =
    gameStateVar.toObservable.map(_.flatMap(AckConfig.forState))

  val playerNames: Signal[TableMap[String]] =
    playerNamesVar.toObservable.map(TableMap.fromMapOrElse(_, _.toString))

  val totalScores: Signal[Map[PlayerPosition, Int]] = totalScoresVar.toObservable

  def playerName(pos: PlayerPosition): String =
    playerNamesVar.now().getOrElse(pos, pos.toString)
}

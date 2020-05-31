package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameStateSpec.RichFullGameState
import io.github.mahh.doko.logic.testutils.CheckersMinHundred
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/**
 * Base trait for test suite of a `FullGameState`.
 *
 * (Those tests are split onto several files because there should be many test cases.)
 */
trait FullGameStateSpec extends AnyFunSuite with CheckersMinHundred {

  implicit def toRichFullGameState(fullGameState: FullGameState): RichFullGameState =
    new RichFullGameState(fullGameState)

}

object FullGameStateSpec {

  class RichFullGameState(private val fullGameState: FullGameState) extends AnyVal {
    def applyActions(actions: (PlayerPosition, PlayerAction[GameState])*): FullGameState = {
      actions.foldLeft(fullGameState) { case (acc, action) =>
        acc.handleAction.applyOrElse(action, (_: (PlayerPosition, PlayerAction[GameState])) => acc)
      }
    }

    def acknowledgedByAll(ack: PlayerAction.Acknowledgement[GameState]): FullGameState = {
      applyActions(PlayerPosition.All.map(_ -> ack): _*)
    }
  }

}
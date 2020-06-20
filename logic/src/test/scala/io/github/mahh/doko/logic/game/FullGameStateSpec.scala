package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameStateSpec.RichFullGameState
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import minitest.SimpleTestSuite
import minitest.laws.Checkers

import scala.language.implicitConversions

/**
 * Base trait for test suite of a `FullGameState`.
 *
 * (Those tests are split onto several files because there should be many test cases.)
 */
trait FullGameStateSpec extends SimpleTestSuite with Checkers with RichFullGameState.Implicits

object FullGameStateSpec {

  class RichFullGameState(private val fullGameState: FullGameState) extends AnyVal {
    def applyActions(actions: (PlayerPosition, PlayerAction[GameState])*): FullGameState = {
      actions.foldLeft(fullGameState) { case (acc, action) =>
        acc.handleAction.applyOrElse(action, (_: (PlayerPosition, PlayerAction[GameState])) => acc)
      }
    }

    def applyActionForAllPLayers(action: PlayerAction[GameState]): FullGameState = {
      applyActions(PlayerPosition.AllAsSet.map(_ -> action).toSeq: _*)
    }

    def acknowledgedByAll(ack: PlayerAction.Acknowledgement[GameState]): FullGameState = {
      applyActions(PlayerPosition.All.map(_ -> ack): _*)
    }
  }

  object RichFullGameState {

    trait Implicits {
      implicit def toRichFullGameState(fullGameState: FullGameState): RichFullGameState =
        new RichFullGameState(fullGameState)
    }

    object Implicits extends Implicits

  }

}
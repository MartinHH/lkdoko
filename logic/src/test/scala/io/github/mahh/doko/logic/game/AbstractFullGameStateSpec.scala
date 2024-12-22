package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.AbstractFullGameStateSpec.RichFullGameState
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

import scala.language.implicitConversions

/**
 * Base class for test suite of a `FullGameState`.
 *
 * (Those tests are split onto several files because there should be many test cases.)
 */
abstract class AbstractFullGameStateSpec[State <: FullGameState](
  protected val defaultGen: Gen[State]
) extends ScalaCheckSuite
     with RichFullGameState.Implicits {

  protected def checkProp(
    name: String,
    gen: Gen[State] = defaultGen
  )(
    prop: State => Prop
  ): Unit = property(name) {
    Prop.forAll(gen)(prop)
  }

}

object AbstractFullGameStateSpec {

  class RichFullGameState(private val fullGameState: FullGameState) extends AnyVal {
    def applyActions(actions: (PlayerPosition, PlayerAction[GameState])*): FullGameState = {
      actions.foldLeft(fullGameState) { case (acc, action) =>
        acc.handleAction.applyOrElse(action, (_: (PlayerPosition, PlayerAction[GameState])) => acc)
      }
    }

    def canApplyActionsProp(actions: (PlayerPosition, PlayerAction[GameState])*): Prop = {
      val (prop, _) = actions.foldLeft[(Prop, FullGameState)](Prop.passed -> fullGameState) {
        case ((propAcc, stateAcc), action) =>
          val p =
            (propAcc && stateAcc.handleAction.isDefinedAt(action) :| s"action=$action")
          p -> applyActions(action)
      }
      prop
    }

    def applyActionForAllPLayers(action: PlayerAction[GameState]): FullGameState = {
      applyActions(PlayerPosition.AllAsSet.map(_ -> action).toSeq*)
    }

    def acknowledgedByAll(ack: PlayerAction.Acknowledgement[GameState]): FullGameState = {
      applyActions(PlayerPosition.All.map(_ -> ack)*)
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

package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.testutils.DeriveArbitrary.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

class ClientStateSpec extends ScalaCheckSuite {

  property("trick is the result of CardConfig.trickForState if GameState is defined") {
    Prop.forAll { (gs: GameState) =>
      val state = ClientState.initial.update(GameStateMessage(gs))
      state.trick ?= CardConfig.trickForState(gs)
    }
  }

  property("hand is the result of CardConfig.handForState if GameState is defined") {
    Prop.forAll { (gs: GameState) =>
      val state = ClientState.initial.update(GameStateMessage(gs))
      state.hand ?= CardConfig.handForState(gs)
    }
  }
}

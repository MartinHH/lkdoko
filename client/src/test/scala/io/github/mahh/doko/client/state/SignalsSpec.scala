package io.github.mahh.doko.client.state

import io.github.mahh.doko.client.testutils.AirstreamSuite
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.testutils.DeriveArbitrary.given
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

class SignalsSpec extends AirstreamSuite {

  propWithOwner("trick propagates the result of CardConfig.trickForState if state is defined") {
    Prop.forAll { (gs: GameState) =>
      val s = new Signals
      s.updateGameState(gs)
      currentValueProp(s.trick)(_ ?= CardConfig.trickForState(gs))
    }
  }

  propWithOwner("hand propagates the result of CardConfig.handForState if state is defined") {
    Prop.forAll { (gs: GameState) =>
      val s = new Signals
      s.updateGameState(gs)
      currentValueProp(s.hand)(_ ?= CardConfig.handForState(gs))
    }
  }
}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.RuleConformingGens.povertyRefusedGen
import io.github.mahh.doko.logic.game.FullGameState.PovertyRefused
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.testutils.GenUtils
import org.scalacheck.Prop

class PovertyRefusedSpec extends AbstractFullGameStateSpec[PovertyRefused](povertyRefusedGen) {

  property("As long as not all players acknowledged, state remains PovertyRefused") {
    // note: defaultGen (i.e. roundResultsGen) always generates the initial state of RoundResults where no player has
    // acknowledged yet
    Prop.forAll(defaultGen, GenUtils.shuffle(PlayerPosition.All)) { (state, posList) =>
      val acknowledgingPlayers = posList.tail
      val newState =
        state.applyActions(acknowledgingPlayers.map(_ -> PlayerAction.AcknowledgeRoundResult)*)
      newState.isInstanceOf[PovertyRefused]
    }
  }

  property("After all players acknowledged, state transitions to Negotiating") {
    Prop.forAll(defaultGen, GenUtils.shuffle(PlayerPosition.All)) { (state, posList) =>
      val newState =
        state.applyActions(posList.map(_ -> PlayerAction.AcknowledgePovertyRefused)*)
      newState.isInstanceOf[FullGameState.Negotiating]
    }
  }
}

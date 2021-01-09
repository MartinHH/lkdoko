package io.github.mahh.doko.logic.game

import org.scalacheck.Prop

class FullGameStateSpec extends AbstractFullGameStateSpec(RuleConformingGens.fullGameStateGen) {

  checkProp("spectatorState.playerState is always empty")(_.spectatorState.playerState.isEmpty)

  checkProp("for all playerStates, playerState is never empty") { gameState =>
    Prop.all(gameState.playerStates.values.map[Prop](_.playerState.nonEmpty): _*)
  }
}

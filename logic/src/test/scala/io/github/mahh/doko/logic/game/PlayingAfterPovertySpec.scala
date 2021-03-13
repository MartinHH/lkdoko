package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.RuleConformingGens._
import org.scalacheck.Prop.AnyOperators

/**
 * Tests on the initial `FullGameState.Playing` after a completed "poverty exchange".
 */
object PlayingAfterPovertySpec extends AbstractFullGameStateSpec(playingAfterPovertyExchangeGen) {

  checkProp("Two players must be Re") { state =>
    state.players.values.count(_.role == Role.Re) ?= 2
  }

  checkProp("Two players must be Kontra") { state =>
    state.players.values.count(_.role == Role.Kontra) ?= 2
  }

  checkProp("trumps must be the appropriate non-solo trumps") { state =>
    val (expected, _) = TrumpsUtil.nonSoloWithSortedHands(state.players.map(_.hand))
    state.trumps ?= expected
  }

}

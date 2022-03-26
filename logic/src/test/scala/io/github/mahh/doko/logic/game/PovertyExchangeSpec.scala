package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.RuleConformingGens._
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Prop.propBoolean

class PovertyExchangeSpec extends AbstractFullGameStateSpec(povertyExchangeGen) {

  checkProp("Number of cards in accepting playerState must be cardsPerPlayer + sizeOfPoverty") { state =>
    import state.rules.deckRule.cardsPerPlayer
    val playerState = state.playerStates(state.acceptingPlayer)
    val nCards = playerState.hand.size
    nCards ?= (cardsPerPlayer + playerState.sizeOfPoverty)
  }

  checkProp("Numbers of cards of non-involved playerStates must be cardsPerPlayer") { state =>
    import state.rules.deckRule.cardsPerPlayer
    Prop.all(
      PlayerPosition.All.filterNot(p => p == state.acceptingPlayer || p == state.poorPlayer).map { pos =>
        val nCards = state.playerStates(pos).hand.size
        (nCards ?= cardsPerPlayer) :| s"pos=$pos"
      }: _*
    )
  }

  checkProp("Total number of cards in playerStates must be 4 * cardsPerPlayer") { state =>
    import state.rules.deckRule.cardsPerPlayer
    state.playerStates.values.map(_.hand.size).sum ?= (cardsPerPlayer * 4)
  }

  checkProp("Poor playerState must not have trumps") { state =>
    state.playerStates(state.poorPlayer).hand.forall(c => !Trumps.Default.isTrump(c))
  }

  property("If the accepting player returns sizeOfPoverty cards, state transitions Playing") {
    Prop.forAll(povertyExchangeFollowUpGen) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.Playing]) :| s"stateOpt: $stateOpt"
    }
  }

}

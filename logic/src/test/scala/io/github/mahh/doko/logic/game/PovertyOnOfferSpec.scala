package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.PovertyOnOffer
import io.github.mahh.doko.logic.game.RuleConformingGens._
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

class PovertyOnOfferSpec extends AbstractFullGameStateSpec[PovertyOnOffer](povertyOnOfferGen) {

  checkProp("If all three player refuse, state is PovertyRefused") { state =>
    val calls = PlayerPosition
      .playingOrder(state.poorPlayer)
      .tail
      .take(3)
      .map(_ -> PlayerAction.PovertyReply(false))
    state.applyActions(calls*).isInstanceOf[FullGameState.PovertyRefused]
  }

  property("If a player that is being offered to accepts, state transitions PovertyExchange") {
    Prop.forAll(povertyOnOfferAcceptedFollowUpGen) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.PovertyExchange]) :| s"stateOpt: $stateOpt"
    }
  }

}

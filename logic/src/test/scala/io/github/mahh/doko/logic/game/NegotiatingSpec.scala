package io.github.mahh.doko.logic.game

import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object NegotiatingSpec extends FullGameStateSpec {

  check("after all four players called one of their possible reservations, state transitions to NegotiationsResult") {
    Prop.forAll(RuleConformingGens.negotiatingAfterFourValidReservationsGen()) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.NegotiationsResult]) :| s"stateOpt: $stateOpt"
    }
  }
}

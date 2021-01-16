package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.NegotiationsResult
import io.github.mahh.doko.logic.game.RuleConformingGens._
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object NegotiationsResultSpec extends FullGameStateSpec[NegotiationsResult](negotiationsResultGen()) {

  private def kontraCount(state: NegotiationsResult): Int = {
    state.players.values.count(_.role == Role.Kontra)
  }

  check("at least two and at most three players are kontra") {
    Prop.forAll(RuleConformingGens.negotiationsResultGen()) { state =>
      val count = kontraCount(state)
      (count >= 2 && count <= 3) :| s"count=$count"
    }
  }

  check("in case of marriage, solo or poverty, the three other players are kontra") {
    Prop.forAll(RuleConformingGens.negotiationsResultGen()) { state =>

      val specialRe: Option[(PlayerPosition, Reservation)] =
        state.result.filterNot { case (_, r) => r == Reservation.Throwing }

      specialRe.nonEmpty ==> {
        val count = kontraCount(state)
        specialRe.map { case (pos, _) => state.players(pos).role }.exists(_ != Role.Kontra) &&
          (count == 3) :| s"count=$count"
      }
    }
  }

  check("after all players acknowledged, state is one of (Playing, PovertyOnOffer, Negotiating)") {
    Prop.forAll(RuleConformingGens.acknowledgedNegotiationsResultGen()) { stateOpt =>
      import FullGameState._
      stateOpt.exists {
        case _: Playing | _: Negotiating | _: PovertyOnOffer => true
        case _ => false
      }
    }
  }

  check("if no player calls poverty of throwing, after all players acknowledged, state is Playing)") {
    import ReservationFilter._
    val gen = RuleConformingGens.acknowledgedNegotiationsResultGen(
      reservationFilter = notThrowing && notPoverty
    )
    Prop.forAll(gen) { stateOpt =>
      import FullGameState._
      stateOpt.exists(_.isInstanceOf[Playing])
    }
  }

  check("if a player calls poverty & no player calls solo nor throwing, after all players acknowledged, " +
    "state is PovertyOnOffer)") {
    val gen = acknowledgedPovertyNegotiationsResultFollowUpGen
    Prop.forAll(gen) { stateOpt =>
      import FullGameState._
      stateOpt.exists(_.isInstanceOf[PovertyOnOffer])
    }
  }

}

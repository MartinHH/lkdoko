package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.NegotiationsResult
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object NegotiationsResultSpec extends FullGameStateSpec {

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

}

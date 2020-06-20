package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.table.TableMapGens
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object NegotiatingSpec extends FullGameStateSpec {

  test("after all four player called one of their possible reservations, state transitions to NegotiationsResult") {
    def genValidCall(s: Negotiating.PlayerState): Gen[PlayerAction.CallReservation] = {
      val all: Seq[Option[Reservation]] = None +: s.reservationState.fold(_.map(Option.apply), _ => Seq.empty)
      Gen.oneOf(all).map(PlayerAction.CallReservation)
    }

    val genAfterFourValidCalls: Gen[Option[FullGameState]] =
      for {
        n <- RuleConformingGens.negotiatingGen()
        calls <- TableMapGens.flatMappedTableMapGen(n.players, genValidCall)
      } yield {
        calls.toMap.foldLeft(Option[FullGameState](n)) { (stateOpt, action) =>
          stateOpt.flatMap(_.handleAction.lift(action))
        }
      }
    check {
      Prop.forAll(genAfterFourValidCalls) { stateOpt =>
        stateOpt.exists(_.isInstanceOf[FullGameState.NegotiationsResult]) :| s"stateOpt: $stateOpt"
      }
    }
  }
}

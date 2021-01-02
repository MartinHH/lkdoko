package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.logic.game.RuleConformingGens._
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean

object NegotiatingSpec extends FullGameStateSpec[Negotiating](negotiatingGen()) {

  private def canCall(
    pos: PlayerPosition,
    state: Negotiating,
    reservation: Reservation
  ): Boolean = state.players(pos).canCall(Some(reservation))

  private def possibleReservationsContains(
    pos: PlayerPosition,
    state: Negotiating,
    reservation: Reservation
  ): Boolean = state.playerStates.get(pos).exists {
    case GameState.AskingForReservations(_, possibleReservations) =>
      possibleReservations.contains(reservation)
    case _ =>
      false
  }

  private def reservationIsAllowed(
    pos: PlayerPosition,
    state: Negotiating,
    reservation: Reservation
  ): Prop = {
    canCall(pos, state, reservation) && possibleReservationsContains(pos, state, reservation)
  }

  private def reservationIsNotAllowed(
    pos: PlayerPosition,
    state: Negotiating,
    reservation: Reservation
  ): Prop = {
    !canCall(pos, state, reservation) && !possibleReservationsContains(pos, state, reservation)
  }

  check("after all four players called one of their possible reservations, state transitions to NegotiationsResult") {
    Prop.forAll(negotiatingAfterFourValidReservationsGen()) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.NegotiationsResult]) :| s"stateOpt: $stateOpt"
    }
  }

  checkProp("any hand allows all solos") { state =>
    Prop.all(
      PlayerPosition.All.flatMap { pos =>
        Reservation.Solo.All.map(reservationIsAllowed(pos, state, _))
      }: _*
    )
  }


  check("if a player has a poverty in hand, the player's possible reservations reflect that") {
    Prop.forAll(withSpecialHand(Dealer.povertyGen, negotiatingGen)) { case (pos, state) =>
      reservationIsAllowed(pos, state, Reservation.Poverty)
    }
  }

  check("if a player has a marriage in hand, the possible reservations reflect that") {
    Prop.forAll(withSpecialHand(Dealer.marriageGen, negotiatingGen)) { case (pos, state) =>
      reservationIsAllowed(pos, state, Reservation.Marriage) && Prop.all(
        PlayerPosition.All.filterNot(_ == pos).map { pos =>
          reservationIsNotAllowed(pos, state, Reservation.Marriage) :| s"$pos cannot call marriage"
        }: _*
      )
    }
  }

  check("if a player has a throwable hand, the player's possible reservations reflect that") {
    Prop.forAll(withSpecialHand(Dealer.throwingGen, negotiatingGen)) { case (pos, state) =>
      reservationIsAllowed(pos, state, Reservation.Throwing)
    }
  }

}

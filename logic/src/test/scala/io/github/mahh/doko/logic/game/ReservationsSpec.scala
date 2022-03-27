package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.*
import io.github.mahh.doko.shared.deck.Rank.*
import io.github.mahh.doko.shared.deck.Suit.*
import io.github.mahh.doko.shared.game.Reservation
import munit.FunSuite

class ReservationsSpec extends FunSuite {

  private def expectReservations(cards: Card*)(expectedNonSolo: Reservation*): Unit = {
    assertEquals(
      Reservations.possibleReservations(cards.toList),
      Reservation.Solo.All ::: expectedNonSolo.toList
    )
  }

  test("poverty of three") {
    expectReservations(
      // format: off
      ♦ | Q, ♦ | Ten, ♦ | K,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | Nine
      // format: on
    )(Reservation.Poverty)
  }

  test("poverty of four") {
    expectReservations(
      // format: off
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine
      // format: on
    )(Reservation.Poverty)
  }

  test("not a poverty of four") {
    expectReservations(
      // format: off
      ♦ | J, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine
      // format: on
    )()
  }

  test("marriage") {
    expectReservations(
      // format: off
      ♣ | Q, ♣ | Q, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K
      // format: on
    )(Reservation.Marriage)
  }

  test("throw due to five nines") {
    expectReservations(
      // format: off
      ♣ | Q, ♣ | J, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine, ♣ | Nine,
      ♠ | Nine, ♠ | Nine
      // format: on
    )(Reservation.Throwing)
  }
}

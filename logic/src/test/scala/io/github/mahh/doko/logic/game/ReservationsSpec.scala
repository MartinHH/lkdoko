package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.*
import io.github.mahh.doko.shared.deck.Rank.*
import io.github.mahh.doko.shared.deck.Suit.*
import io.github.mahh.doko.shared.game.Reservation
import munit.FunSuite

class ReservationsSpec extends FunSuite {

  private def expectReservations(cards: Card*)(expectedNonSolo: Reservation*): Unit = {
    assertEquals(Reservations.possibleReservations(cards.toList), Reservation.Solo.All ::: expectedNonSolo.toList)
  }

  test("poverty of three") {
    expectReservations(
      ♦ | Q, ♦ | Ten, ♦ | K,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | Nine
    )(Reservation.Poverty)
  }

  test("poverty of four") {
    expectReservations(
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine
    )(Reservation.Poverty)
  }

  test("not a poverty of four") {
    expectReservations(
      ♦ | J, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine
    )()
  }

  test("marriage") {
    expectReservations(
      ♣ | Q, ♣ | Q, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K
    )(Reservation.Marriage)
  }

  test("throw due to five nines") {
    expectReservations(
      ♣ | Q, ♣ | J, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine, ♣ | Nine,
      ♠ | Nine, ♠ | Nine
    )(Reservation.Throwing)
  }
}

package io.github.mahh.doko.shared.rules

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import minitest.SimpleTestSuite

object TrumpsSpec extends SimpleTestSuite {

  private[this] def ordersAsExpected(trumps: Trumps)(expected: Card*): Unit = {
    val sorted = Card.allBySuit.reverse.sorted(trumps.cardsOrdering)
    assertEquals(sorted, expected.toList)
  }

  test("Default.cardsOrdering must order cards as expected") {
    ordersAsExpected(Trumps.Default)(
      ♥ | Ten,
      ♣ | Q, ♠ | Q, ♥ | Q, ♦ | Q,
      ♣ | J, ♠ | J, ♥ | J, ♦ | J,
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | A, ♥ | K, ♥ | Nine
    )
  }

  test("Piglets.cardsOrdering must order cards as expected") {
    ordersAsExpected(Trumps.Piglets)(
      ♦ | A,
      ♥ | Ten,
      ♣ | Q, ♠ | Q, ♥ | Q, ♦ | Q,
      ♣ | J, ♠ | J, ♥ | J, ♦ | J,
      ♦ | Ten, ♦ | K, ♦ | Nine,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | A, ♥ | K, ♥ | Nine
    )
  }

  test("QueensSolo.cardsOrdering must order cards as expected") {
    ordersAsExpected(Trumps.Solo.QueensSolo)(
      ♣ | Q, ♠ | Q, ♥ | Q, ♦ | Q,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | J, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | J, ♠ | Nine,
      ♥ | A, ♥ | Ten, ♥ | K, ♥ | J, ♥ | Nine,
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | J, ♦ | Nine
    )
  }

  test("JacksSolo.cardsOrdering must order cards as expected") {
    ordersAsExpected(Trumps.Solo.JacksSolo)(
      ♣ | J, ♠ | J, ♥ | J, ♦ | J,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Q, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Q, ♠ | Nine,
      ♥ | A, ♥ | Ten, ♥ | K, ♥ | Q, ♥ | Nine,
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Q, ♦ | Nine
    )
  }

  test("ClubsSolo.cardsOrdering must order cards as expected") {
    ordersAsExpected(Trumps.Solo.ClubsSolo)(
      ♣ | Q, ♠ | Q, ♥ | Q, ♦ | Q,
      ♣ | J, ♠ | J, ♥ | J, ♦ | J,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | A, ♥ | Ten, ♥ | K, ♥ | Nine,
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Nine
    )
  }
}

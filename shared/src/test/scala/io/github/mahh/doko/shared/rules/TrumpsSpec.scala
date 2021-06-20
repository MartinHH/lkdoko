package io.github.mahh.doko.shared.rules

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.rules.Trumps.Solo.SuitSolo
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
      ♥ | Ten,
      ♣ | Q, ♠ | Q, ♥ | Q, ♦ | Q,
      ♣ | J, ♠ | J, ♥ | J, ♦ | J,
      ♣ | A, ♣ | Ten, ♣ | K, ♣ | Nine,
      ♠ | A, ♠ | Ten, ♠ | K, ♠ | Nine,
      ♥ | A,          ♥ | K, ♥ | Nine,
      ♦ | A, ♦ | Ten, ♦ | K, ♦ | Nine
    )
  }

  test("there are four suit-solos") {
    assertEquals(Trumps.Solo.All.collect { case s: SuitSolo[_] => s }.toSet.size, 4)
  }

  test("♥10 is highest card in all suit-solos") {
    Trumps.Solo.All.collect { case s: SuitSolo[_] => s }.foreach { solo =>
      assertEquals(Card.allBySuit.min(solo.cardsOrdering), ♥ | Ten)
    }
  }
}

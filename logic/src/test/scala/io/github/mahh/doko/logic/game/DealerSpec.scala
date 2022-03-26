package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.rules.DeckRule
import munit.FunSuite

class DealerSpec extends FunSuite {

  private def cardsPerPlayer(deckRule: DeckRule, expected: Int): Unit = {
    assert(Dealer.dealtCards(deckRule).values.forall(_.size == expected))
    assert(deckRule.cardsPerPlayer == expected)
  }

  private def packSize(deckRule: DeckRule, expected: Int): Unit = {
    assertEquals(Dealer.randomPack(deckRule).size, expected)
  }

  test("dealt cards (with nines) must contain 12 cards for each player") {
    cardsPerPlayer(DeckRule.WithNines, 12)
  }

  test("dealt cards (without nines) must contain 10 cards for each player") {
    cardsPerPlayer(DeckRule.WithoutNines, 10)
  }

  test("A full pack (with nines) contains 48 cards") {
    packSize(DeckRule.WithNines, 48)
  }

  test("A full pack (without nines) contains 40 cards") {
    packSize(DeckRule.WithoutNines, 40)
  }

  test("The total value of a full pack cards must be 240") {
    DeckRule.all.foreach { dr =>
      assertEquals(Dealer.randomPack(dr).map(_.value).sum, 240)
    }

  }
}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.Dealer.CardsPerPlayer
import minitest.SimpleTestSuite

object DealerSpec extends SimpleTestSuite {

  test("dealt cards must contain 12 cards for each player") {
    assert(Dealer.dealtCards.values.forall(_.size == CardsPerPlayer))
  }

  test("A full pack contains 48 cards") {
    assertEquals(Dealer.randomPack.size, 48)
  }

  test("The total value of a full pack cards must be 240") {
    assertEquals(Dealer.randomPack.map(_.value).sum, 240)
  }
}

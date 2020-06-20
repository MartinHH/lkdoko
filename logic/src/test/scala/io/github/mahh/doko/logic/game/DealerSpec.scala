package io.github.mahh.doko.logic.game

import minitest.SimpleTestSuite

object DealerSpec extends SimpleTestSuite {

  test("dealt cards must contain 12 cards for each player") {
    assert(Dealer.dealtCards.values.forall(_.size == CardsPerPlayer))
  }

}

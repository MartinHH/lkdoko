package io.github.mahh.doko.logic.game

import org.scalatest.funsuite.AnyFunSuite

class DealtCardsSpec extends AnyFunSuite {

  test("dealt cards must contain 12 cards for each player") {
    assert(dealtCards.values.forall(_.size === CardsPerPlayer))
  }

}

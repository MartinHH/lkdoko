package io.github.mahh.doko.logic.game

import org.scalatest.funsuite.AnyFunSuite

class DealtCardsSpec extends AnyFunSuite {

  test("dealt cards must contain cards for all players") {
    assert(dealtCards.size === 4)
  }

  test("dealt cards must contain 12 cards for each player") {
    assert(dealtCards.values.forall(_.size === CardsPerPlayer))
  }

}

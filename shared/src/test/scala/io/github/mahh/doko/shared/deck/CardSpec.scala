package io.github.mahh.doko.shared.deck

import org.scalatest.funsuite.AnyFunSuite

class CardSpec extends AnyFunSuite {

  test("allByRank must contain the same cards as allBySuit") {
    assert(
      Card.allByRank.size === Card.allBySuit.size &&
        Card.allBySuit.toSet === Card.allByRank.toSet)
  }

  test("The total value of all cards must be 120") {
    assert(Card.allByRank.map(_.value).sum === 120)
  }

  test("A full pack contains 48 cards") {
    assert(Card.randomPack.size === 48)
  }

  test("The total value of a full pack cards must be 240") {
    assert(Card.randomPack.map(_.value).sum === 240)
  }

}

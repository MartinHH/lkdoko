package io.github.mahh.doko.shared.deck

import minitest.SimpleTestSuite

object CardSpec extends SimpleTestSuite {

  test("allByRank must contain the same cards as allBySuit") {
    assertEquals(Card.allByRank.size, Card.allBySuit.size)
    assertEquals(Card.allByRank.toSet, Card.allBySuit.toSet)
  }

  test("The total value of all cards must be 120") {
    assertEquals(Card.allByRank.map(_.value).sum, 120)
  }

}

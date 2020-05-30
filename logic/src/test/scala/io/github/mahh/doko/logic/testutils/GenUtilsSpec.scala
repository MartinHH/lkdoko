package io.github.mahh.doko.logic.testutils

import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalatest.funsuite.AnyFunSuite

class GenUtilsSpec extends AnyFunSuite with CheckersMinHundred {

  test("shuffle generates on of each input values into the result") {
    val input = 0 to 10
    val gen = GenUtils.shuffle(input)
    val expected = input.toSet
    check {
      Prop.forAll(gen) { shuffled =>
        shuffled.toSet ?= expected
      }
    }
  }
}

package io.github.mahh.doko.shared.testutils

import minitest.SimpleTestSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

object GenUtilsSpec extends SimpleTestSuite with Checkers {

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

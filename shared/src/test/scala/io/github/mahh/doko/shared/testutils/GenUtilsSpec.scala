package io.github.mahh.doko.shared.testutils

import munit.ScalaCheckSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

class GenUtilsSpec extends ScalaCheckSuite {

  property("shuffle generates on of each input values into the result") {
    val input = 0 to 10
    val gen = GenUtils.shuffle(input)
    val expected = input.toSet
    Prop.forAll(gen) { shuffled =>
      shuffled.toSet ?= expected
    }
  }
}

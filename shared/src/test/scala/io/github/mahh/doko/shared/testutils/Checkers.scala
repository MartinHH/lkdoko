package io.github.mahh.doko.shared.testutils

import minitest.SimpleTestSuite
import org.scalacheck.Prop
import org.scalacheck.Test
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.Params

/**
 * Scalacheck integration.
 *
 * Inspired by minitest-laws, but with Pretty-Params (and increased default-verbosity) and seed-printing.
 */
trait Checkers {

  self: SimpleTestSuite =>

  protected def minSuccessfulTests: Int = Test.Parameters.default.minSuccessfulTests

  protected def checkParams: Parameters = Test.Parameters.default.withMinSuccessfulTests(minSuccessfulTests)

  protected def prettyParams: Params = Params(1)

  protected def check(
    name: String,
    config: Parameters = checkParams,
    prettyParams: Params = prettyParams
  )(
    prop: => Prop
  ): Unit = {
    test(name) {
      val result = Test.check(config, Prop.delay(prop).viewSeed(name))
      if (!result.passed) {
        val reason = Pretty.pretty(result, prettyParams)
        fail(reason)
      }
    }
  }

}

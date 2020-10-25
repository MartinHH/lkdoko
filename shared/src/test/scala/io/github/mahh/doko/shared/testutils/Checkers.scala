package io.github.mahh.doko.shared.testutils

import minitest.api.Asserts.fail
import org.scalacheck.Prop
import org.scalacheck.Test
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.Params

/**
 * Scalacheck integration.
 *
 * Inspired by minitest-laws, but with Pretty-Params (and increased default-verbosity).
 */
trait Checkers {

  def checkParams: Parameters = Test.Parameters.default

  def prettyParams: Params = Params(1)

  def check(prop: Prop, config: Parameters = checkParams, prettyParams: Params = prettyParams): Unit = {
    val result = Test.check(config, prop)
    val reason = Pretty.pretty(result, prettyParams)
    if (!result.passed) fail(reason)
  }

}

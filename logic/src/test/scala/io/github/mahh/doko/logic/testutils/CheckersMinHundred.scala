package io.github.mahh.doko.logic.testutils

import org.scalatestplus.scalacheck.Checkers


/**
 * `Checkers` with minimum successful test set (back) up to 100.
 */
trait CheckersMinHundred extends Checkers {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

}

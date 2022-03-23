package io.github.mahh.doko.shared.msg

import io.github.mahh.doko.shared.json.Json
import io.github.mahh.doko.shared.testutils.Checkers
import minitest.SimpleTestSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

abstract class MessageSpec[M: Json.Encoder : Json.Decoder : Arbitrary](msgName: String)
  extends SimpleTestSuite with Checkers {

  check(s"$msgName can be encoded and then decoded") {
    Prop.forAll { (msg: M) =>
      Json.decode[M](Json.encode(msg)) ?= Right(msg)
    }
  }
}

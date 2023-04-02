package io.github.mahh.doko.shared.json

import io.github.mahh.doko.shared.json.Json.*
import io.github.mahh.doko.shared.json.Protocol.given
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.martinhh.derived.arbitrary.given
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

class ProtocolSpec extends ScalaCheckSuite {

  def test[M: Encoder: Decoder: Arbitrary](msgName: String): Unit =
    property(s"$msgName can be encoded and then decoded") {
      Prop.forAll { (msg: M) =>
        Json.decode[M](Json.encode(msg)) ?= Right(msg)
      }
    }

  test[MessageToClient]("MessageToClient")

  test[MessageToServer]("MessageToServer")
}

package io.github.mahh.doko.logic.table.client

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

given Arbitrary[ClientId] = Arbitrary(Gen.uuid.map(ClientId.apply))

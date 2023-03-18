package io.github.mahh.doko.logic.table.participant

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

given Arbitrary[ParticipantId] = Arbitrary(Gen.uuid.map(ParticipantId.apply))

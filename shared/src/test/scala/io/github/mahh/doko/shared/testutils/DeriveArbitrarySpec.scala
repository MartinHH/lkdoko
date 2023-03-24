package io.github.mahh.doko.shared.testutils

import io.github.mahh.doko.shared.testutils.DeriveArbitrary.derived
import munit.FunSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

class DeriveArbitrarySpec extends FunSuite {

  import DeriveArbitrarySpec._

  private def equalValues[T](expectedGen: Gen[T])(using derivedArb: Arbitrary[T]): Unit = {
    (0 until 100).foldLeft(Seed.random()) { case (seed, _) =>
      val expected = expectedGen(Parameters.default, seed)
      val derived = derivedArb.arbitrary(Parameters.default, seed)
      assertEquals(derived, expected, s"Differing values for seed $seed")
      seed.next
    }
  }

  test("Generates same values as non-derived Gen (for simple case class)") {
    equalValues(SimpleCaseClass.expectedGen)
  }

  test("Generates same values as non-derived Gen (for simple ADT)") {
    equalValues(SimpleADT.expectedGen)
  }
}

object DeriveArbitrarySpec {

  sealed trait SimpleADT

  object SimpleADT {
    val expectedGen: Gen[SimpleADT] =
      Gen.oneOf(Gen.const(SimpleCaseObject), SimpleCaseClass.expectedGen)
  }

  case object SimpleCaseObject extends SimpleADT
  case class SimpleCaseClass(x: Int, y: Int, s: Option[String]) extends SimpleADT

  object SimpleCaseClass {
    val expectedGen: Gen[SimpleCaseClass] =
      for {
        x <- arbitrary[Int]
        y <- arbitrary[Int]
        s <- arbitrary[Option[String]]
      } yield SimpleCaseClass(x, y, s)
  }

}

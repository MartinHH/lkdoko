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

  test("Generates same values as non-derived Gen (for simple Enum)") {
    equalValues(ABC.expectedGen)
  }

  test("Generates same values as non-derived Gen (for more complex example)") {
    equalValues(ComplexADTWithNestedMembers.expectedGen)
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

  enum ABC(asChar: Char) {
    case A extends ABC('A')
    case B extends ABC('B')
    case C extends ABC('C')
  }

  object ABC {
    val expectedGen: Gen[ABC] =
      Gen.oneOf(ABC.A, ABC.B, ABC.C)
  }

  sealed trait ComplexADTWithNestedMembers

  case object AnotherCaseObject extends ComplexADTWithNestedMembers

  sealed abstract class AbstractSubClass[N <: SimpleADT](abc: ABC)
    extends ComplexADTWithNestedMembers {
    def nestedSimple: N
  }

  object AbstractSubClass {
    case class SubclassA(a: Int, b: String, nestedSimple: SimpleCaseClass)
      extends AbstractSubClass[SimpleCaseClass](ABC.A)

    object SubclassA {
      val expectedGen: Gen[SubclassA] =
        for {
          a <- arbitrary[Int]
          b <- arbitrary[String]
          n <- SimpleCaseClass.expectedGen
        } yield SubclassA(a, b, n)
    }

    case class SubclassB(nestedSimple: SimpleADT) extends AbstractSubClass[SimpleADT](ABC.B)

    object SubclassB {
      val expectedGen: Gen[SubclassB] =
        SimpleADT.expectedGen.map(SubclassB.apply)
    }
    case class SubclassC(c: String, d: Double, anotherLetter: ABC)
      extends AbstractSubClass[SimpleCaseObject.type](ABC.C) {
      override def nestedSimple: SimpleCaseObject.type = SimpleCaseObject
    }

    object SubclassC {
      val expectedGen: Gen[SubclassC] =
        for {
          c <- arbitrary[String]
          d <- arbitrary[Double]
          l <- ABC.expectedGen
        } yield SubclassC(c, d, l)
    }
  }

  object ComplexADTWithNestedMembers {
    val expectedGen: Gen[ComplexADTWithNestedMembers] =
      Gen.oneOf(
        Gen.const(AnotherCaseObject),
        AbstractSubClass.SubclassA.expectedGen,
        AbstractSubClass.SubclassB.expectedGen,
        AbstractSubClass.SubclassC.expectedGen
      )
  }
}

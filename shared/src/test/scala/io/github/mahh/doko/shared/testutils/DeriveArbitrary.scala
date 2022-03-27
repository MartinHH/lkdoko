package io.github.mahh.doko.shared.testutils

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.*

/**
 * Simple support for derivation of `Arbitrary`s.
 *
 * @note There's also [[https://github.com/alexarchambault/scalacheck-shapeless]] for this purpose,
 *       but that is not available for scala 3.
 */
object DeriveArbitrary {

  inline private def summonAll[T <: Tuple]: Tuple =
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  => summonInline[Arbitrary[t]] *: summonAll[ts]

  inline private def arbitrarySum[T](s: Mirror.SumOf[T]): Arbitrary[T] =
    val elems = summonAll[s.MirroredElemTypes]
    elems.toList.asInstanceOf[List[Arbitrary[T]]] match
      case arb :: Nil => arb
      case gen0 :: gen1 :: gens =>
        Arbitrary(Gen.oneOf(gen0.arbitrary, gen1.arbitrary, gens.map(_.arbitrary): _*))
      case Nil => Arbitrary(Gen.fail)

  inline private def arbitraryTuple[T <: Tuple]: Arbitrary[Tuple] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        Arbitrary(Gen.const(EmptyTuple))
      case _: (t *: ts) =>
        val genT = for {
          tVal <- summonInline[Arbitrary[t]].arbitrary
          tsVal <- arbitraryTuple[ts].arbitrary
        } yield tVal *: tsVal
        Arbitrary(genT)

  inline private def arbitraryProduct[T](p: Mirror.ProductOf[T]): Arbitrary[T] =
    Arbitrary(arbitraryTuple[p.MirroredElemTypes].arbitrary.map(p.fromProduct(_)))

  inline given derived[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match
      case s: Mirror.SumOf[T]     => arbitrarySum(s)
      case p: Mirror.ProductOf[T] => arbitraryProduct(p)

  // implicit resolution fails with some strange error if we don't provide this:
  inline given listArb[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary.arbContainer[List, T]

}

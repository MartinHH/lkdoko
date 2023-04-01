package io.github.mahh.doko.shared.testutils

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.erasedValue
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.deriving.*

/**
 * Simple support for derivation of `Arbitrary`s.
 *
 * @note There's also [[https://github.com/alexarchambault/scalacheck-shapeless]] for this purpose,
 *       but that is not available for scala 3.
 */
object DeriveArbitrary {

  /**
   * Helper for deriving `Gen`s for all subclasses of a "sum" (including sealed sub-traits).
   */
  private trait GensList[+T]:
    def gens: List[Gen[T]]

  private trait LowPriorityGensListGivens {
    inline given nonSumGensList[T](using a: Arbitrary[T]): GensList[T] =
      new GensList[T]:
        def gens: List[Gen[T]] = List(a.arbitrary)
  }

  private object GensList extends LowPriorityGensListGivens {

    inline given derived[T](using s: Mirror.SumOf[T]): GensList[T] =
      val elems = summonAll[Tuple.Map[s.MirroredElemTypes, GensList]]
      val elemsList = elems.toList.asInstanceOf[List[GensList[T]]]
      val gs: List[Gen[T]] = elemsList.flatMap(_.gens)
      new GensList[T]:
        def gens: List[Gen[T]] = gs

  }

  inline private def arbitrarySum[T](s: Mirror.SumOf[T]): Arbitrary[T] =
    val gens = summonInline[GensList[T]].gens
    Arbitrary(Gen.choose(0, gens.size - 1).flatMap(i => gens(i)))

  inline private def genTuple[T <: Tuple]: Gen[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        Gen.const(EmptyTuple.asInstanceOf[T])
      case _: (t *: ts) =>
        for {
          tVal <- summonInline[Arbitrary[t]].arbitrary
          tsVal <- genTuple[ts]
        } yield (tVal *: tsVal).asInstanceOf[T]

  inline private def arbitraryProduct[T](p: Mirror.ProductOf[T]): Arbitrary[T] =
    Arbitrary(genTuple[p.MirroredElemTypes].map(p.fromProduct(_)))

  inline given derived[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match
      case s: Mirror.SumOf[T]     => arbitrarySum(s)
      case p: Mirror.ProductOf[T] => arbitraryProduct(p)

}

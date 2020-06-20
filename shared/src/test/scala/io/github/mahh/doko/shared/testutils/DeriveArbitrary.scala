package io.github.mahh.doko.shared.testutils

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import shapeless._
import shapeless.ops.coproduct.Length
import shapeless.ops.nat.ToInt

/**
 * Simple support for shapeless based derivation of `Arbitrary`s.
 *
 * @note There's also [[https://github.com/alexarchambault/scalacheck-shapeless]] for this purpose,
 *       but this is simple, does the job and avoids yet another external dependency.
 */
object DeriveArbitrary extends ProductTypeClassCompanion[Arbitrary] {

  object typeClass extends ProductTypeClass[Arbitrary] {

    override def product[H, T <: HList](ch: Arbitrary[H], ct: Arbitrary[T]): Arbitrary[H :: T] = Arbitrary {
      for {
        h <- ch.arbitrary
        t <- ct.arbitrary
      } yield h :: t
    }

    override def emptyProduct: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))

    override def project[F, G](instance: => Arbitrary[G], to: F => G, from: G => F): Arbitrary[F] = Arbitrary {
      instance.arbitrary.map(from)
    }

    def coproduct[L, R <: Coproduct, N <: Nat](
      cl: => Arbitrary[L],
      cr: => Arbitrary[R],
      rl: => Length.Aux[R, N],
      toInt: => ToInt[N]
    ): Arbitrary[L :+: R] = Arbitrary {
      Gen.frequency(
        1 -> cl.arbitrary.map(Inl(_)),
        toInt() -> cr.arbitrary.map(Inr(_))
      )
    }


    def emptyCoproduct: Arbitrary[CNil] = Arbitrary(Gen.fail)

  }

  implicit def deriveCNil: Arbitrary[CNil] = typeClass.emptyCoproduct

  implicit def deriveCCons[H, T <: Coproduct, N <: Nat](
    implicit ch: Lazy[Arbitrary[H]],
    ct: Lazy[Arbitrary[T]],
    rt: Lazy[Length.Aux[T, N]],
    toInt: Lazy[ToInt[N]]
  ): Arbitrary[H :+: T] = typeClass.coproduct(ch.value, ct.value, rt.value, toInt.value)

}

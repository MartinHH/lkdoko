package io.github.mahh.doko.shared.testutils

import org.scalacheck.Gen

object GenUtils {

  /**
   * Keeps taking arbitrary elements from `input` until the remainder satisfies `stopTaking`.
   */
  def takeSomeUntil[A](input: Seq[A])(stopTaking: Seq[A] => Boolean): Gen[List[A]] = {

    def takeSome(input: Vector[A], suffix: List[A]): Gen[List[A]] = {
      if (stopTaking(input)) {
        Gen.const(suffix)
      } else {
        Gen.choose(0, input.size - 1).flatMap { i =>
          val (prefix, tail) = input.splitAt(i)
          takeSome(prefix ++ tail.tail, tail.head :: suffix)
        }
      }
    }

    takeSome(input.toVector, List.empty)
  }

  def shuffle[A](input: Seq[A]): Gen[List[A]] = {
    takeSomeUntil[A](input)(_.isEmpty)
  }
}

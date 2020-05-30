package io.github.mahh.doko.logic.testutils

import org.scalacheck.Gen

object GenUtils {

  def shuffle[A](input: Seq[A]): Gen[List[A]] = {

    def shuffle(input: Vector[A], suffix: List[A]): Gen[List[A]] = {
      if(input.isEmpty) {
        Gen.const(suffix)
      } else {
        Gen.choose(0, input.size - 1).flatMap { i =>
          val (prefix, tail) = input.splitAt(i)
          shuffle(prefix ++ tail.tail, tail.head :: suffix)
        }
      }
    }

    shuffle(input.toVector, List.empty)
  }
}

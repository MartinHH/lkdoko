package io.github.mahh.doko.shared.deck.ops

import io.github.mahh.doko.shared.deck.{ Card, Rank, Suit }

import scala.language.implicitConversions

class SuitOps[S <: Suit](private val s: S) extends AnyVal {
  def |[R <: Rank](r: R): Card = Card(s, r)
}

object SuitOps {
  trait Implicits {
    implicit def toSuitOps[S <: Suit](s: S): SuitOps[S] = new SuitOps(s)
  }

  object Implicits extends Implicits
}

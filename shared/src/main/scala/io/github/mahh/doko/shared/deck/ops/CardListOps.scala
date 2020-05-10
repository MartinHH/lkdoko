package io.github.mahh.doko.shared.deck.ops

import io.github.mahh.doko.shared.deck.{ Card, Rank, Suit }

import scala.language.implicitConversions

class CardListOps[S <: Suit, R <: Rank](private val cards: List[Card]) extends AnyVal {

  def without(ranks: Rank*): List[Card] = {
    val rankSet = ranks.toSet
    cards.filterNot(c => rankSet.contains(c.rank))
  }

  def withoutQueensAndJacks: List[Card] = without(Rank.Q, Rank.J)

}

object CardListOps {

  trait Implicits {
    implicit def toCardListOps[S <: Suit, R <: Rank](cards: List[Card]): CardListOps[S, R] =
      new CardListOps(cards)
  }

  object Implicits extends Implicits
}

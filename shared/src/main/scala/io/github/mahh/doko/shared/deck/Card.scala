package io.github.mahh.doko.shared.deck

import io.github.mahh.doko.shared.json.Json._

case class Card(suit: Suit, rank: Rank) derives Encoder, Decoder {
  override def toString: String = f"[$suit${rank.shortName}%2s]"

  def value: Int = rank.value
}

object Card {
  val allByRank: List[Card] = for {
    r <- Rank.all
    s <- Suit.all
  } yield Card(s, r)

  val allBySuit: List[Card] = for {
    s <- Suit.all
    r <- Rank.all
  } yield Card(s, r)

}

package io.github.mahh.doko.shared.deck

import scala.util.Random

case class Card(suit: Suit, rank: Rank) {
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

  val fullPack: List[Card] = allBySuit ::: allBySuit

  def randomPack: List[Card] = Random.shuffle(fullPack)
}
package io.github.mahh.doko.shared

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck.ops.CardListOps
import io.github.mahh.doko.shared.deck.ops.SuitOps

package object deck
  extends SuitOps.Implicits
  with CardListOps.Implicits {

  def allOfRank[R <: Rank](r: R): List[Card] = Suit.all.map(Card(_, r))

  def allOfSuit[S <: Suit](s: S): List[Card] = Rank.all.map(Card(s, _))

  val Aces: List[Card] = allOfRank(A)
  val Tens: List[Card] = allOfRank(Ten)
  val Kings: List[Card] = allOfRank(K)
  val Queens: List[Card] = allOfRank(Q)
  val Jacks: List[Card] = allOfRank(J)
  val Nines: List[Card] = allOfRank(Nine)

  val Hearts10: Card = ♥ | Ten

  val Fox: Card = ♦ | A

  val Charly: Card = ♣ | J

  val QueenOfClubs: Card = ♣ | Q

  val Piglets: List[Card] = List(Fox, Fox)

  val TotalDeckValue: Int = Card.allByRank.map(_.value).sum * 2

}

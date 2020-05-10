package io.github.mahh.doko.shared.deck

sealed abstract class Suit

object Suit {

  case object ♣ extends Suit

  case object ♠ extends Suit

  case object ♥ extends Suit

  case object ♦ extends Suit

  type ♣ = ♣.type
  type ♠ = ♠.type
  type ♥ = ♥.type
  type ♦ = ♦.type

  def clubs: ♣ = ♣

  def spades: ♠ = ♠

  def hearts: ♥ = ♥

  def diamonds: ♦ = ♦

  val all: List[Suit] = List(♣, ♠, ♥, ♦)
}

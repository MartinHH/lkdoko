package io.github.mahh.doko.shared.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.player.PlayerPosition

/**
 * Aka "Stich".
 *
 * @param trickStarter The player that played (or will be playing) the first card.
 * @param cards  All cards of the trick that have been played so far.
 */
case class Trick(
  trickStarter: PlayerPosition,
  cards: Map[PlayerPosition, Card]
) {
  def isComplete: Boolean = cards.size == PlayerPosition.TotalNumberOfPlayers

  def currentPlayer: Option[PlayerPosition] = {
    PlayerPosition.playingOrder(trickStarter).take(PlayerPosition.TotalNumberOfPlayers).lift(cards.size)
  }

  def isEmpty: Boolean = cards.isEmpty
}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Card.allBySuit
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

import scala.util.Random

object Dealer {

  val fullPack: List[Card] = allBySuit ::: allBySuit

  def randomPack(implicit deckRule: DeckRule): List[Card] = Random.shuffle(deckRule.fullPack)

  /**
   * Shuffles a pack to the four players.
   *
   * @param shuffledPack The shuffled pack of 48 cards (or less if some cards are already pre-dealt
   *                     via `mapToFill`.
   * @param mapToFill The map of cards for each player that shall be dealt to. Usually an empty map,
   *                  but for testing purposes, this may be pre-filled. In that case, the pre-filled
   *                  cards must be removed from `shuffledPack`.
   */
  private[game] def dealtCards(
    shuffledPack: List[Card],
    mapToFill: TableMap[Seq[Card]] = TableMap.fill(Vector.empty[Card])
  )(
    implicit deckRule: DeckRule
  ): TableMap[Seq[Card]] = {
    val (_, result) =
      PlayerPosition.All.foldLeft((shuffledPack, mapToFill)) {
        case ((cards, acc), pos) =>
          val existing = acc(pos)
          val (forPlayer, remaining) = cards.splitAt(deckRule.cardsPerPlayer - existing.size)
          (remaining, acc + (pos -> (existing ++ forPlayer)))
      }

    result
  }

  private[game] def dealtCards(implicit deckRule: DeckRule): TableMap[Seq[Card]] = {
    dealtCards(randomPack)
  }

}

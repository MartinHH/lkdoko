package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Card.allBySuit
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

import scala.util.Random

object Dealer {

  val fullPack: List[Card] = allBySuit ::: allBySuit

  def randomPack: List[Card] = Random.shuffle(fullPack)

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
  ): TableMap[Seq[Card]] = {
    val (_, result) =
      PlayerPosition.All.foldLeft((shuffledPack, mapToFill)) {
        case ((cards, acc), pos) =>
          val existing = acc(pos)
          val (forPlayer, remaining) = cards.splitAt(CardsPerPlayer - existing.size)
          (remaining, acc + (pos -> (existing ++ forPlayer)))
      }

    result
  }

  private[game] def dealtCards: TableMap[Seq[Card]] = dealtCards(randomPack)

  val CardsPerPlayer = 12
}

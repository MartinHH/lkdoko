package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.CardsPerPlayer
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

object Dealer {

  private[game] def dealtCards(shuffledPack: List[Card]): TableMap[List[Card]] = {
    val (_, result) =
      PlayerPosition.All.foldLeft((shuffledPack, TableMap.fill(List.empty[Card]))) {
        case ((cards, acc), pos) =>
          val (forPlayer, remaining) = cards.splitAt(CardsPerPlayer)
          (remaining, acc + (pos -> forPlayer))
      }

    result
  }

  private[game] def dealtCards: TableMap[List[Card]] = dealtCards(Card.randomPack)
}

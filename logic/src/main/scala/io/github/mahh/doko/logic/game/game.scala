package io.github.mahh.doko.logic

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

package object game {

  val CardsPerPlayer = 12

  val MarriageRounds = 3

  private[game] def dealtCards: TableMap[List[Card]] = {
    val (_, result) =
      PlayerPosition.All.foldLeft((Card.randomPack, TableMap.fill(List.empty[Card]))) {
        case ((cards, acc), pos) =>
          val (forPlayer, remaining) = cards.splitAt(CardsPerPlayer)
          (remaining, acc + (pos -> forPlayer))
      }

    result
  }

}

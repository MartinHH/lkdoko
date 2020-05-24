package io.github.mahh.doko.logic

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.player.PlayerPosition

package object game {

  val CardsPerPlayer = 12

  val MarriageRounds = 3

  private[game] def dealtCards: Map[PlayerPosition, List[Card]] = {
    val (_, result) =
      PlayerPosition.All.foldLeft((Card.randomPack, Map.empty[PlayerPosition, List[Card]])) {
        case ((cards, acc), pos) =>
          val (forPlayer, remaining) = cards.splitAt(CardsPerPlayer)
          (remaining, acc + (pos -> forPlayer))
      }

    result
  }

}

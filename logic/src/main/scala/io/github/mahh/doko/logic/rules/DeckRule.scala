package io.github.mahh.doko.logic.rules

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Card.allBySuit
import io.github.mahh.doko.shared.deck.Rank

/**
 * Describes which cards are used for playing.
 */
sealed abstract class DeckRule(filter: Rank => Boolean) {

  val fullPack: List[Card] = (allBySuit ::: allBySuit).filter(card => filter(card.rank))

  val cardsInPack: Int = fullPack.size

  val cardsPerPlayer: Int = cardsInPack / 4
}

object DeckRule {

  case object WithNines extends DeckRule(_ => true)

  case object WithoutNines extends DeckRule(_ != Rank.Nine)

  val all: List[DeckRule] = List(WithNines, WithoutNines)
}

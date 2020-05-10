package io.github.mahh.doko.logic.player

import io.github.mahh.doko.shared.deck.Card

case class PlayerState(
  initialCards: List[Card],
  currentCards: List[Card],
  wonTricks: List[Card]
)

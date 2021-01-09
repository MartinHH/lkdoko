package io.github.mahh.doko.logic.rules

import io.github.mahh.doko.logic.rules

/**
 * Configurable rules.
 */
sealed trait Rules {
  implicit val deckRule: DeckRule
}

object Rules {

  private case class Rules(deckRule: DeckRule) extends rules.Rules

  def apply(implicit deckRule: DeckRule): rules.Rules = Rules(deckRule)

}

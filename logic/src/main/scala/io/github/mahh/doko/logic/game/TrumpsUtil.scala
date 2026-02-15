package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.table.TableMap

private[logic] object TrumpsUtil {

  private def trumps(isPiglets: Boolean): Trumps.NonSolo = {
    if (isPiglets) Trumps.Piglets else Trumps.Default
  }

  /**
   * Chooses between `Trumps.Piglets` and `Trumps.Default` and sorts input cards accordingly.
   */
  def nonSoloWithSortedHands(
    unsortedHands: TableMap[Seq[Card]]
  ): (Trumps.NonSolo, TableMap[Seq[Card]]) = {
    val trumps = TrumpsUtil.trumps(unsortedHands.values.exists(_.count(_ == Fox) > 1))
    trumps -> unsortedHands.map(_.sorted(using trumps.cardsOrdering))
  }

  /**
   * Chooses between `Trumps.Piglets` and `Trumps.Default` returns the corresponding ordering
   */
  def nonSoloOrdering(
    hand: Seq[Card]
  ): Ordering[Card] = {
    trumps(hand.count(_ == Fox) > 1).cardsOrdering
  }

}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.table.TableMap

private[logic] object TrumpsUtil {

  /**
   * Chooses between `Trumps.Piglets` and `Trumps.Default` and sorts input cards accordingly.
   */
  def nonSoloWithSortedHands(
    unsortedHands: TableMap[Seq[Card]]
  ): (Trumps.NonSolo, TableMap[Seq[Card]]) = {
    val isPiglets = unsortedHands.values.exists(_.count(_ == Fox) > 1)
    val trumps = if (isPiglets) Trumps.Piglets else Trumps.Default
    trumps -> unsortedHands.map(_.sorted(trumps.cardsOrdering))
  }

}

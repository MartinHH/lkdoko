package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Hearts10
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps

/** Logic related to a single trick. */
object TrickAnalyzer {

  def playableCards(
    hands: Map[PlayerPosition, Seq[Card]],
    trick: Trick,
    trumps: Trumps
  ): Map[PlayerPosition, Set[Card]] = {
    val currentPlayer = trick.currentPlayer
    hands.map { case (pos, hand) =>
      val playable: Set[Card] =
        if (currentPlayer.contains(pos)) {
          trick.cards.get(trick.trickStarter).fold {
            hand.toSet
          } { toServe =>
            def filteredOrElse(p: Card => Boolean): Set[Card] = {
              val filtered = hand.filter(p)
              val resultSeq = if (filtered.isEmpty) hand else filtered
              resultSeq.toSet
            }

            val p: Card => Boolean =
              if (trumps.isTrump(toServe)) {
                trumps.isTrump
              } else {
                c => !trumps.isTrump(c) && c.suit == toServe.suit
              }
            filteredOrElse(p)
          }
        } else {
          Set.empty
        }
      pos -> playable
    }
  }

  def winner(
    trick: Trick,
    trumps: Trumps
  ): Option[PlayerPosition] = {
    if (trick.isComplete) {
      val trickOrder = PlayerPosition.trickOrder(trick.trickStarter)
      val byRegularOrdering: PlayerPosition =
        trickOrder.minBy(trick.cards(_))(trumps.cardsOrdering)
      val winner =
        if (trick.cards(byRegularOrdering) == Hearts10 && trick.cards.values.count(_ == Hearts10) > 1) {
          trickOrder.filter(trick.cards.get(_).contains(Hearts10)).last
        } else {
          byRegularOrdering
        }
      Some(winner)
    } else {
      None
    }
  }
}

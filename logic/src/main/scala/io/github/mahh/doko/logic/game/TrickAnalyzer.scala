package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Hearts10
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.table.TableMap

/** Logic related to a single trick. */
object TrickAnalyzer {

  def playableCards(
    hands: TableMap[Seq[Card]],
    trick: Trick,
    trumps: Trumps
  ): TableMap[Set[Card]] = {
    val currentPlayer = trick.currentPlayer
    hands.mapWithPos { (pos, hand) =>
      val playable: Set[Card] =
        if (currentPlayer.contains(pos)) {
          trick.cards
            .get(trick.trickStarter)
            .fold {
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
                } else { c =>
                  !trumps.isTrump(c) && c.suit == toServe.suit
                }
              filteredOrElse(p)
            }
        } else {
          Set.empty
        }
      playable
    }
  }

  def winner(
    trick: CompleteTrick,
    trumps: Trumps
  ): PlayerPosition = {
    val trickOrder = PlayerPosition.trickOrder(trick.trickStarter)
    val startingCard = trick.cards(trick.trickStarter)
    val possibleWinners = trickOrder.filter { p =>
      val card = trick.cards(p)
      card.suit == startingCard.suit || trumps.isTrump(card)
    }
    val byRegularOrdering: PlayerPosition =
      possibleWinners.minBy(trick.cards)(trumps.cardsOrdering)
    val winner =
      if (
        trick.cards(byRegularOrdering) == Hearts10
        && trick.cards.values.count(_ == Hearts10) > 1
      ) {
        trickOrder.filter(trick.cards(_) == Hearts10).last
      } else {
        byRegularOrdering
      }
    winner
  }
}

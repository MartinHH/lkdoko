package io.github.mahh.doko.shared.rules

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.utils.OrderingFromSeq

/**
 * Describes which cards are trump and how cards are ranked.
 *
 * @param trumpsInOrder All cards that are trump in order of their ranking (highest first).
 */
sealed abstract class Trumps(protected val trumpsInOrder: List[Card]) {

  val isTrump: Card => Boolean = trumpsInOrder.toSet

  val trumpsOrdering: Ordering[Card] = OrderingFromSeq(trumpsInOrder)

  val cardsOrdering: Ordering[Card] = OrderingFromSeq(trumpsInOrder ::: Card.allBySuit.filterNot(isTrump))

}

object Trumps {

  sealed trait NonSolo extends Trumps

  private val default: List[Card] =
    Hearts10 :: Queens ::: Jacks ::: allOfSuit(♦).withoutQueensAndJacks

  /**
   * Regular trumps.
   */
  case object Default extends Trumps(default) with NonSolo

  /**
   * If one player has both aces of diamonds (2 "foxes" become "piglets"), they rank higher than any other card.
   */
  case object Piglets extends Trumps(Fox :: Default.trumpsInOrder.filterNot(_ == Fox)) with NonSolo

  /**
   * Trumps that may be selected by a player who plays a "solo".
   */
  sealed trait Solo extends Trumps

  object Solo {

    private def suitSolo[S <: Suit](suit: S): List[Card] =
      (Hearts10 :: Queens ::: Jacks ::: allOfSuit(suit).withoutQueensAndJacks).distinct

    sealed abstract class SuitSolo[S <: Suit](suit: S) extends Trumps(suitSolo(suit)) with Solo

    case object ClubsSolo extends SuitSolo(♣)

    case object SpadesSolo extends SuitSolo(♠)

    case object HeartsSolo extends SuitSolo(♥)

    case object DiamondsSolo extends SuitSolo(♦)

    sealed abstract class CourtSolo[C <: Rank](jackOrQueen: C) extends Trumps(allOfRank(jackOrQueen)) with Solo

    case object QueensSolo extends CourtSolo(Q)

    case object JacksSolo extends CourtSolo(J)

    // TODO: this should be a val, but there seem to be some strange initialization order problems
    //  due to which sometimes members of this are null if this is a val - these should be analyzed
    //  and fixed differently
    def All: List[Solo] = List(QueensSolo, JacksSolo, ClubsSolo, SpadesSolo, HeartsSolo, DiamondsSolo)

  }

}

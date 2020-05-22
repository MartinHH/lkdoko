package io.github.mahh.doko.shared.score

import io.github.mahh.doko.shared.bids.WinningBid.BidExtension

/**
 * Scores that can be achieved during a round of doppelkopf.
 *
 * @param value The value of each score.
 */
sealed abstract class Score(val value: Int)

object Score {

  case object Won extends Score(1)

  case object AgainstTheElders extends Score(1)

  case object WinCalled extends Score(2)

  case class PlayedBelow(limit: BidExtension) extends Score(1)

  case class PlayedBelowCalled(limit: BidExtension) extends Score(1)

  case class PlayedBelowCalledByOpponent(limit: BidExtension) extends Score(1)

  /**
   * A score that is not the result of winning enough tricks, but achieved by some special action.
   */
  sealed trait SpecialScore extends Score

  case object FoxCaught extends Score(1) with SpecialScore

  case object Charly extends Score(1) with SpecialScore

  case object CharlyCaught extends Score(1) with SpecialScore

  case object DoKo extends Score(1) with SpecialScore {
    val minValue = 40
  }

}

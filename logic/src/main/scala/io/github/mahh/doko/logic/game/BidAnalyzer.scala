package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.TeamAnalyzer.TeamWithBid
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

/** Logic to track which bids can be called by each player. */
object BidAnalyzer {


  /**
   * Returns the lowest possible bids that may be called by each player (if any).
   */
  def nextPossibleBids(
    currentTrick: Trick,
    wonTricks: List[(PlayerPosition, CompleteTrick)],
    roles: TableMap[Role],
    bids: Map[PlayerPosition, Bid]
  ): Map[PlayerPosition, Bid] = {

    val roleExists: Role => Boolean = roles.values.toSet

    if (roleExists(Role.Marriage) && !roleExists(Role.Married)) {
      // "finding phase" of marriage - no bids can be called yet
      Map.empty
    } else {
      // number of tricks that were played before bidding started
      val delayOfBiddingPhase: Int =
        if (roleExists(Role.MarriageSolo)) {
          // marriage did not find a partner - bidding started after MarriageRounds:
          MarriageRounds
        } else {
          val roundsTillMarriage: Option[Int] =
            for {
              partner <- roles.toMap.collectFirst { case (p, Role.Married) => p }
              findingTrick <- wonTricks.reverse.zipWithIndex.collectFirst { case ((w, _), i) if w == partner => i }
            } yield findingTrick + 1
          // either a marriage happened or bidding started from first trick:
          roundsTillMarriage.getOrElse(0)
        }
      val bidsToDrop: Int = {
        val tricksPlayedSinceStartOfBidding = wonTricks.size - delayOfBiddingPhase
        val oneOrLessCardsPlayedInTrick = currentTrick.cards.sizeCompare(1) <= 0
        if (oneOrLessCardsPlayedInTrick) tricksPlayedSinceStartOfBidding - 1 else tricksPlayedSinceStartOfBidding
      }
      val stillAllowed = Bid.All.drop(bidsToDrop)

      val (elders, others) = TeamAnalyzer.splitTeamsWithBids(roles, bids)

      def teamResult(teamWithBid: TeamWithBid): Map[PlayerPosition, Bid] = {
        val (team, bid) = teamWithBid
        val nextBid = stillAllowed.dropWhile(b => bid.exists(Bid.ordering.gteq(_, b))).headOption
        nextBid.fold[Map[PlayerPosition, Bid]](Map.empty) { b =>
          team.map(_ -> b).toMap
        }
      }

      teamResult(elders) ++ teamResult(others)
    }

  }
}

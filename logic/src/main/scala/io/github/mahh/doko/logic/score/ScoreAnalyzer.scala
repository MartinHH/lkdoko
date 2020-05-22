package io.github.mahh.doko.logic.score

import io.github.mahh.doko.logic.game.Role
import io.github.mahh.doko.logic.game.TeamAnalyzer
import io.github.mahh.doko.shared.bids.WinningBid.Bid
import io.github.mahh.doko.shared.bids.WinningBid.BidExtension
import io.github.mahh.doko.shared.deck.Charly
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.deck.TotalDeckValue
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Score.SpecialScore
import io.github.mahh.doko.shared.score.Scores

object ScoreAnalyzer {

  private[score] def getSpecialScores(
    tricks: List[(PlayerPosition, Trick)],
    team: Set[PlayerPosition]
  ): List[SpecialScore] = {
    val charlyScores = tricks.headOption.fold[List[SpecialScore]](List.empty) { case (w, t) =>
      if (team(w)) {
        val charly = if (t.cards.get(w).contains(Charly)) List(Score.Charly) else List.empty[SpecialScore]
        val charlyCaught = t.cards.toList.collect {
          case (p, Charly) if p != w && !team(p) => Score.CharlyCaught
        }
        charly ::: charlyCaught
      } else {
        List.empty
      }
    }
    tricks.foldLeft(charlyScores) {
      case (acc, (w, _)) if !team(w) =>
        acc
      case (acc, (_, t)) =>
        val isDoko = t.cards.values.map(_.value).sum > Score.DoKo.minValue
        val foxes = t.cards.toList.collect {
          case (p, Fox) if !team(p) => Score.FoxCaught
        }
        val scores = if (isDoko) Score.DoKo :: foxes else foxes
        scores ::: acc
    }
  }

  private[score] def tricksValue(
    tricks: List[(PlayerPosition, Trick)],
    team: Set[PlayerPosition]
  ): Int = {
    tricks.collect { case (w, t) if team(w) => t.cards.values.map(_.value).sum }.sum
  }

  private[score] def winnerScores(
    isAgainstElders: Boolean,
    opponentsValue: Int,
    ownBid: Option[Bid],
    opponentsBid: Option[Bid]
  ): List[Score] = {
    def bidScores(bid: Option[Bid])(f: BidExtension => Score): List[Score] = {
      val extension = bid.collect {case b: BidExtension => b}
      bid.map(_ => Score.WinCalled) ++:
        BidExtension.All.filter(b => extension.exists(_.limit <= b.limit)).map(f)
    }

    List(
      Some(Score.Won),
      if (isAgainstElders) Some(Score.AgainstTheElders) else None,
      BidExtension.All.filter(_.limit >= opponentsValue).map(Score.PlayedBelow.apply),
      bidScores(ownBid)(Score.PlayedBelowCalled.apply),
      bidScores(opponentsBid)(Score.PlayedBelowCalledByOpponent.apply)
    ).flatten
  }


  def scores(
    bids: Map[PlayerPosition, Bid],
    tricks: List[(PlayerPosition, Trick)],
    roles: Map[PlayerPosition, Role]
  ): Scores = {
    val ((elders, eldersBid), (others, othersBid)) = TeamAnalyzer.splitTeamsWithBids(roles, bids)

    val valueOfElders = tricksValue(tricks, elders)
    val valueOfOthers = TotalDeckValue - valueOfElders


    val eldersExtension = eldersBid.collect { case b: BidExtension => b }
    val othersExtension = othersBid.collect { case b: BidExtension => b }

    val eldersAreWinners = valueOfElders > TotalDeckValue / 2 && !eldersExtension.exists(_.limit < valueOfOthers)
    val othersAreWinners = !eldersAreWinners && !othersExtension.exists(_.limit < valueOfElders)

    val eldersSpecialScores = getSpecialScores(tricks, elders)
    val othersSpecialScores = getSpecialScores(tricks, others)

    val eldersWinnerScore =
      if (eldersAreWinners) {
        winnerScores(isAgainstElders = false, valueOfOthers, eldersBid, othersBid)
      } else {
        List.empty
      }

    val othersWinnerScore =
      if (othersAreWinners) {
        winnerScores(isAgainstElders = elders.size > 1, valueOfElders, othersBid, eldersBid)
      } else {
        List.empty
      }

    Scores(
      Scores.TeamScore(
        elders,
        eldersWinnerScore ::: eldersSpecialScores,
        valueOfElders
      ),
      Scores.TeamScore(
        others,
        othersWinnerScore ::: othersSpecialScores,
        valueOfOthers
      )
    )
  }
}

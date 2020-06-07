package io.github.mahh.doko.logic.score

import io.github.mahh.doko.logic.game.Role
import io.github.mahh.doko.logic.game.TeamAnalyzer
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.BidExtension
import io.github.mahh.doko.shared.deck.Charly
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.deck.TotalDeckValue
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Score.SpecialScore
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.table.TableMap

/** Logic to calculate the final scores of a round. */
object ScoreAnalyzer {

  private[this] def charlyScores(
    lastTrick: (PlayerPosition, CompleteTrick),
    team: Set[PlayerPosition]
  ): List[SpecialScore] = {
    val (w, t) = lastTrick
    if (team(w)) {
      val charly = if (t.cards(w) == Charly) List(Score.Charly) else List.empty[SpecialScore]
      val charlyCaught = t.cards.collect {
        case (p, Charly) if p != w && !team(p) => Score.CharlyCaught
      }.toList
      charly ::: charlyCaught
    } else {
      List.empty
    }
  }

  private[score] def getSpecialScores(
    tricks: List[(PlayerPosition, CompleteTrick)],
    team: Set[PlayerPosition]
  ): List[SpecialScore] = {
    val charlies = tricks.headOption.fold(List.empty[SpecialScore])(charlyScores(_, team))
    tricks.foldLeft(charlies) {
      case (acc, (w, _)) if !team(w) =>
        acc
      case (acc, (_, t)) =>
        val isDoko = t.cards.values.map(_.value).sum > Score.DoKo.minValue
        val foxes = t.cards.collect {
          case (p, Fox) if !team(p) => Score.FoxCaught
        }.toList
        val scores = if (isDoko) Score.DoKo :: foxes else foxes
        scores ::: acc
    }
  }

  private[score] def tricksValue(
    tricks: List[(PlayerPosition, CompleteTrick)],
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
      val extension = bid.collect { case b: BidExtension => b }
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
    tricks: List[(PlayerPosition, CompleteTrick)],
    roles: TableMap[Role]
  ): Scores = {
    val ((elders, eldersBid), (others, othersBid)) = TeamAnalyzer.splitTeamsWithBids(roles, bids)

    val valueOfElders = tricksValue(tricks, elders)
    // should be 240-valueOfElders, but we count here to make possible bugs more obvious detected:
    val valueOfOthers = tricksValue(tricks, others)


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

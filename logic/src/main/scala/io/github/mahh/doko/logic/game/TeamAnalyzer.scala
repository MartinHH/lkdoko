package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.player.PlayerPosition

object TeamAnalyzer {

  type TeamWithBid = (Set[PlayerPosition], Option[Bid])

  def splitTeams(
    roles: Map[PlayerPosition, Role]
  ): (Set[PlayerPosition], Set[PlayerPosition]) = {
    val elders: Set[PlayerPosition] = {
      val marriage = roles.collectFirst { case (p, Role.Marriage) => p }
      if (marriage.nonEmpty) {
        val elders = marriage.toSet ++ roles.collectFirst { case (p, Role.Married) => p }
        elders
      } else {
        val solo = roles.collectFirst { case (p, Role.SilentMarriage | Role.MarriageSolo | Role.Solo(_)) => p }
        if (solo.nonEmpty) {
          solo.toSet
        } else {
          roles.collect { case (p, Role.Re) => p }.toSet
        }
      }
    }
    val others: Set[PlayerPosition] = roles.keySet -- elders
    elders -> others
  }

  def splitTeamsWithBids(
    roles: Map[PlayerPosition, Role],
    bids: Map[PlayerPosition, Bid]
  ): (TeamWithBid, TeamWithBid) = {
    val (elders, others) = splitTeams(roles)
    def withBid(team: Set[PlayerPosition]): TeamWithBid = team -> team.map(bids.get).maxOption.flatten

    withBid(elders) -> withBid(others)
  }

}

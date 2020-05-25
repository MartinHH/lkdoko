package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

/** Logic to track which players are on the same team (and what their current combined bid is). */
object TeamAnalyzer {

  type TeamWithBid = (Set[PlayerPosition], Option[Bid])

  def splitTeams(
    roles: TableMap[Role]
  ): (Set[PlayerPosition], Set[PlayerPosition]) = {
    val elders: Set[PlayerPosition] = {
      val rolesMap = roles.toMap
      val marriage = rolesMap.collectFirst { case (p, Role.Marriage) => p }
      if (marriage.nonEmpty) {
        val elders = marriage.toSet ++ rolesMap.collectFirst { case (p, Role.Married) => p }
        elders
      } else {
        val solo = rolesMap.collectFirst { case (p, Role.SilentMarriage | Role.MarriageSolo | Role.Solo(_)) => p }
        if (solo.nonEmpty) {
          solo.toSet
        } else {
          rolesMap.collect { case (p, Role.Re) => p }.toSet
        }
      }
    }
    val others: Set[PlayerPosition] = roles.keySet -- elders
    elders -> others
  }

  def splitTeamsWithBids(
    roles: TableMap[Role],
    bids: Map[PlayerPosition, Bid]
  ): (TeamWithBid, TeamWithBid) = {
    val (elders, others) = splitTeams(roles)
    def withBid(team: Set[PlayerPosition]): TeamWithBid = team -> team.map(bids.get).maxOption.flatten

    withBid(elders) -> withBid(others)
  }

}

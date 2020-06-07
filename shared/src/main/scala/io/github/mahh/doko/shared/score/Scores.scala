package io.github.mahh.doko.shared.score

import cats.instances.all._
import cats.syntax.all._
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Scores.TeamScore

/**
 * The final scores of one round.
 *
 * @param eldersScore The scores of the "elders" team.
 * @param othersScore The scores of the other team.
 */
case class Scores(
  eldersScore: TeamScore,
  othersScore: TeamScore
) {
  def all: List[TeamScore] = List(eldersScore, othersScore)

  def totals: List[Int] = {
    val maxTeamsSize = all.map(_.team.size).max
    val diff = (eldersScore.total - othersScore.total) * maxTeamsSize
    val eldersTotal = diff / eldersScore.team.size
    val othersTotal = - diff / othersScore.team.size
    List(eldersTotal, othersTotal)
  }

  def totalsPerPlayer: Map[PlayerPosition, Int] = all.zip(totals).foldMap {
    case (teamScore, total) => teamScore.team.map(_ -> total).toMap
  }
}

object Scores {

  /**
   * Final scores of one round for one team.
   *
   * @param team The members of the team.
   * @param scores The scores achieved by the team.
   * @param tricksValue The total value of the tricks that were won by the team.
   */
  case class TeamScore(team: Set[PlayerPosition], scores: List[Score], tricksValue: Int) {
    def total: Int = scores.foldMap(_.value)
  }

}

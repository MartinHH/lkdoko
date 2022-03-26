package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.RoundResults
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.Scores.TeamScore
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import RuleConformingGens._

class RoundResultsSpec extends AbstractFullGameStateSpec[RoundResults](roundResultsGen()) {

  private def teamSize(team: Scores => TeamScore, min: Int, max: Int)(result: FullGameState.RoundResults): Prop = {
    val teamSize = team(result.scores).team.size
    teamSize >= min && teamSize <= max
  }

  private def totalOccurrencesOfScore(score: Score, max: Int)(result: FullGameState.RoundResults): Prop = {
    result.scores.all.flatMap(_.scores).count(_ == score) <= max
  }

  checkProp("games based on a regular deck result in total trick-values of 240") { result =>
    result.scores.all.map(_.tricksValue).sum ?= 240
  }

  checkProp("sum of scores is always 0") { result =>
    result.scores.totalsPerPlayer.values.sum ?= 0
  }

  checkProp("total teams size is always 4") { result =>
    (result.scores.eldersScore.team.size + result.scores.othersScore.team.size) ?= 4
  }

  checkProp("elders team size is always 1 or 2") {
    teamSize(_.eldersScore, 1, 2)
  }

  checkProp("other team size is always 2 or 3") {
    teamSize(_.othersScore, 2, 3)
  }

  checkProp(
    "in case of a solo, elders team size is 1",
    roundResultsGen(reservationFilter = ReservationFilter.soloOnly)
  ) {
    teamSize(_.eldersScore, min = 1, max = 1)
  }

  checkProp("there's at most one winner team") {
    totalOccurrencesOfScore(Score.Won, max = 1)
  }

  checkProp("there's at most two fox caught scores") {
    totalOccurrencesOfScore(Score.FoxCaught, max = 2)
  }

  checkProp("there's at most two charly scores") {
    totalOccurrencesOfScore(Score.Charly, max = 2)
  }

  checkProp("there's at most two charly caught scores") {
    totalOccurrencesOfScore(Score.CharlyCaught, max = 2)
  }
}

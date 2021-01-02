package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.RoundResults
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.Scores.TeamScore
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import RuleConformingGens._

object RoundResultsSpec extends FullGameStateSpec[RoundResults](roundResultsGen()) {

  private def teamSize(team: Scores => TeamScore, min: Int, max: Int)(result: FullGameState.RoundResults): Prop = {
    val teamSize = team(result.scores).team.size
    teamSize >= min && teamSize <= max
  }

  checkProp("games based on a regular deck result in total trick-values of 240") { result =>
    result.scores.all.map(_.tricksValue).sum ?= 240
  }

  checkProp("sum of scores is always 0") { result =>
    result.scores.totalsPerPlayer.values.sum ?= 0
  }

  checkProp("elders team size is always 1 or 2") {
    teamSize(_.eldersScore, 1, 2)
  }

  checkProp("other team size is always 2 or 3") {
    teamSize(_.othersScore, 2, 3)
  }

  checkProp("there's at most one winner team") { result =>
    result.scores.all.count(_.scores.contains(Score.Won)) <= 1
  }

}

package io.github.mahh.doko.shared.score

import io.github.mahh.doko.shared.bids.Bid.BidExtension.No90
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition.*
import io.github.mahh.doko.shared.score.Scores.TeamScore
import munit.FunSuite

class ScoresSpec extends FunSuite {

  test("regular totals are calculated correctly") {
    val elders: Set[PlayerPosition] = Set(Player1, Player4)
    val scores = {
      val soloScores = TeamScore(elders, Nil, 110)
      val othersScores = TeamScore(AllAsSet -- elders, List(Score.Won, Score.AgainstTheElders), 130)
      Scores(soloScores, othersScores)
    }
    val expectedTotals: Map[PlayerPosition, Int] = AllAsSet.map(p => p -> (if (elders(p)) -2 else 2)).toMap
    assertEquals(scores.totalsPerPlayer, expectedTotals)
  }

  test("solo totals are calculated correctly") {
    val soloPlayer: PlayerPosition = Player1
    val scores = {
      val soloScores = TeamScore(Set(soloPlayer), Nil, 89)
      val othersScores = TeamScore(AllAsSet - soloPlayer, List(Score.Won, Score.PlayedBelow(No90)), 151)
      Scores(soloScores, othersScores)
    }
    val expectedTotals: Map[PlayerPosition, Int] = Map(soloPlayer -> -6) ++ (AllAsSet - soloPlayer).map(_ -> 2)
    assertEquals(scores.totalsPerPlayer, expectedTotals)
  }
}

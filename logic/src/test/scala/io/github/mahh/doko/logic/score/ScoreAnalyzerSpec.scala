package io.github.mahh.doko.logic.score

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import org.scalatest.funsuite.AnyFunSuite

class ScoreAnalyzerSpec extends AnyFunSuite {

  test("if winner of last trick played jack of diamonds, she scored Charly") {
    import PlayerPosition._
    val trick = Trick(
      Player1,
      Map(
        Player1 -> (♣ | A),
        Player2 -> (♣ | Nine),
        Player3 -> (♣ | Nine),
        Player4 -> (♣ | J),
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player4 -> trick), Set(Player1, Player4))
    assert(scores === List(Score.Charly))
  }

  test("if winner of last trick did not play jack of diamonds, but her teammate did, they did not score Charly") {
    import PlayerPosition._
    val trick = Trick(
      Player1,
      Map(
        Player1 -> (♣ | Q),
        Player2 -> (♣ | Nine),
        Player3 -> (♣ | Nine),
        Player4 -> (♣ | J),
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player1 -> trick), Set(Player1, Player4))
    assert(scores === Nil)
  }

  test("if winner of last trick catches jack of diamonds from the other team, she scored CharlyCaught") {
    import PlayerPosition._
    val trick = Trick(
      Player1,
      Map(
        Player1 -> (♣ | Q),
        Player2 -> (♣ | J),
        Player3 -> (♣ | Nine),
        Player4 -> (♣ | J),
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player1 -> trick), Set(Player1, Player3))
    assert(scores === List(Score.CharlyCaught, Score.CharlyCaught))
  }


}

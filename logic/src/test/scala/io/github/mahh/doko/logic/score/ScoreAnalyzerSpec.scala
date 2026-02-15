package io.github.mahh.doko.logic.score

import io.github.mahh.doko.shared.deck.*
import io.github.mahh.doko.shared.deck.Rank.*
import io.github.mahh.doko.shared.deck.Suit.*
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition.Player1
import io.github.mahh.doko.shared.player.PlayerPosition.Player2
import io.github.mahh.doko.shared.player.PlayerPosition.Player3
import io.github.mahh.doko.shared.player.PlayerPosition.Player4
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.table.TableMap
import io.github.martinhh.derived.arbitrary.given
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop

class ScoreAnalyzerSpec extends ScalaCheckSuite {

  test("if winner of last trick played jack of diamonds, she scored Charly") {
    val trick = CompleteTrick(
      Player1,
      TableMap(
        ♣ | A,
        ♣ | Nine,
        ♣ | Nine,
        ♣ | J
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player4 -> trick), Set(Player1, Player4))
    assertEquals(scores, List(Score.Charly))
  }

  test(
    "if winner of last trick did not play jack of diamonds, but her teammate did, they did not score Charly"
  ) {
    val trick = CompleteTrick(
      Player1,
      TableMap(
        ♣ | Q,
        ♣ | Nine,
        ♣ | Nine,
        ♣ | J
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player1 -> trick), Set(Player1, Player4))
    assertEquals(scores, Nil)
  }

  test(
    "if winner of last trick catches jack of diamonds from the other team, she scored CharlyCaught"
  ) {
    val trick = CompleteTrick(
      Player1,
      TableMap(
        ♣ | Q,
        ♣ | J,
        ♣ | Nine,
        ♣ | J
      )
    )
    val scores = ScoreAnalyzer.getSpecialScores(List(Player1 -> trick), Set(Player1, Player3))
    assertEquals(scores, List(Score.CharlyCaught, Score.CharlyCaught))
  }

  test("if another trick than the last was won via jack of diamonds, no one scored Charly") {
    val previousTrick = CompleteTrick(
      Player1,
      TableMap(
        ♣ | A,
        ♣ | Nine,
        ♣ | Nine,
        ♣ | J
      )
    )

    val lastTrick = CompleteTrick(
      Player1,
      TableMap(
        ♠ | A,
        ♠ | Nine,
        ♠ | Nine,
        ♠ | J
      )
    )
    val scores =
      ScoreAnalyzer.getSpecialScores(
        List(Player4 -> lastTrick, Player4 -> previousTrick),
        Set(Player1, Player4)
      )
    assertEquals(scores, List.empty)
  }

  property("if team wins ace of diamonds from the other team, they scored 'fox caught'") {
    val foxCaughtTrick = Player4 -> CompleteTrick(
      Player1,
      TableMap(
        ♣ | A,
        ♦ | A,
        ♣ | Nine,
        ♣ | J
      )
    )

    def scores(allTricks: List[(PlayerPosition, CompleteTrick)]): List[Score.SpecialScore] =
      ScoreAnalyzer.getSpecialScores(allTricks, Set(Player3, Player4))

    Prop.forAll { (tricks: List[(PlayerPosition, CompleteTrick)]) =>

      // position of trick in game should not matter, so let's check two positions:
      val resultIfTrickWasLast = scores(foxCaughtTrick :: tricks)
      val resultIfTrickWasFirst = scores(tricks :+ foxCaughtTrick)

      resultIfTrickWasLast.contains(Score.FoxCaught) &&
      resultIfTrickWasFirst.contains(Score.FoxCaught)
    }
  }

  test("if a player wins ace of diamonds from her teammate, they did not score 'fox caught'") {
    val foxCaughtTrick = Player4 -> CompleteTrick(
      Player1,
      TableMap(
        ♣ | A,
        ♦ | A,
        ♣ | Nine,
        ♦ | J
      )
    )

    assert(ScoreAnalyzer.getSpecialScores(List(foxCaughtTrick), Set(Player2, Player4)).isEmpty)
  }
}

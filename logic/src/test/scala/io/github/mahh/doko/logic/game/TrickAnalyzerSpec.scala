package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.player.PlayerPosition._
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.table.TableMap
import org.scalatest.funsuite.AnyFunSuite

class TrickAnalyzerSpec extends AnyFunSuite {

  test("second ten of hearts wins over first") {
    val trick = CompleteTrick(
      Player2,
      TableMap(
        ♥ | Ten,
        ♠ | A,
        ♠ | Q,
        ♥ | Ten
      )
    )
    // special case: if both ♥ | Ten are played in the same trick
    assert(TrickAnalyzer.winner(trick, Trumps.Default) == Player1)
  }

  test("besides ten of hearts, first card wins over second (of the same type)") {
    val trick = CompleteTrick(
      Player1,
      TableMap(
        ♠ | Ten,
        ♠ | A,
        ♠ | Q,
        ♠ | Q
      )
    )
    // ♠ | Q is highest, first one wins
    assert(TrickAnalyzer.winner(trick, Trumps.Default) == Player3)
  }

  test("non-trump cards not matching the initial suit never wins") {
    val trick = CompleteTrick(
      Player2,
      TableMap(
        ♠ | A,
        ♦ | K,
        ♦ | Q,
        ♦ | K
      )
    )
    // ace of spades has higher value, but still doesn't win because it is not trump
    assert(TrickAnalyzer.winner(trick, Trumps.Solo.JacksSolo) == Player2)
  }
}

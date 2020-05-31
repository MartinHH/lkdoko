package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Playing.PlayerState
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgeTrickResult
import io.github.mahh.doko.shared.player.PlayerAction.PlayCard
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition._
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap

class PlayingSpec extends FullGameStateSpec {

  test("in case of 'marriage', if another player wins the first trick, she marries the marriage player") {
    // game just started, player 1 has a marriage, player 2 starts the game
    val initial =
      FullGameState.Playing(
        Player2,
        TableMap(
          PlayerState(
            List(♣ | Q, ♣ | Q, ♥ | Q, ♦ | Q, ♠ | J, ♦ | Ten, ♣ | Ten, ♣ | Nine, ♠ | Ten, ♠ | Nine, ♥ | K, ♥ | Nine),
            Role.Marriage, None
          ),
          PlayerState(
            List(♠ | Q, ♥ | Q, ♣ | J, ♦ | J, ♦ | Ten, ♦ | Nine, ♣ | A, ♣ | Nine, ♠ | Ten, ♠ | Nine, ♥ | K, ♥ | Nine),
            Role.Kontra, None
          ),
          PlayerState(
            List(♥ | Ten, ♠ | Q, ♠ | J, ♥ | J, ♥ | J, ♦ | A, ♦ | K, ♦ | Nine, ♣ | A, ♣ | K, ♠ | A, ♠ | A),
            Role.Kontra, None
          ),
          PlayerState(
            List(♥ | Ten, ♦ | Q, ♣ | J, ♦ | J, ♦ | A, ♦ | K, ♣ | Ten, ♣ | K, ♠ | K, ♠ | K, ♥ | A, ♥ | A),
            Role.Kontra, None
          )
        ),
        Some(Player1 -> Reservation.Marriage),
        Trumps.Default,
        Trick(Player2, Map.empty),
        TotalScores(List())
      )
    // player2 wins the first trick:
    val afterTrick = initial.applyActions(
      Player2 -> PlayCard(♣ | A),
      Player3 -> PlayCard(♣ | K),
      Player4 -> PlayCard(♣ | K),
      Player1 -> PlayCard(♣ | Ten)
    ).acknowledgedByAll(AcknowledgeTrickResult)

    // player 2 should now have role "married":
    val expectedRoles = TableMap(Role.Marriage, Role.Married, Role.Kontra, Role.Kontra)
    assert(afterTrick.isInstanceOf[FullGameState.Playing])
    assert(afterTrick.asInstanceOf[FullGameState.Playing].players.map(_.role) == expectedRoles)
  }
}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.BidExtension
import io.github.mahh.doko.shared.deck.*
import io.github.mahh.doko.shared.deck.Rank.*
import io.github.mahh.doko.shared.deck.Suit.*
import io.github.mahh.doko.shared.game.CompleteTrick
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition.*
import io.github.mahh.doko.shared.table.TableMap
import munit.FunSuite

class BidAnalyzerSpec extends FunSuite {

  private val dummyFirstTrick: (PlayerPosition.Player3.type, CompleteTrick) =
    Player3 -> CompleteTrick(
      Player1,
      TableMap(
        ♥ | A,
        ♥ | Nine,
        ♦ | A,
        ♥ | Nine
      )
    )

  // roles of a regular game: 2 players are re, 2 are kontra:
  private val dummyRegularRoles: TableMap[Role] = TableMap(
    player1Val = Role.Re,
    player2Val = Role.Re,
    player3Val = Role.Kontra,
    player4Val = Role.Kontra
  )

  test(
    "in a regular game, teams can call win until (including) the first card of the second trick"
  ) {
    val currentTrick = Trick(
      Player3,
      Map(
        Player3 -> (♣ | A)
      )
    )
    val result = BidAnalyzer.nextPossibleBids(
      currentTrick,
      List(dummyFirstTrick),
      dummyRegularRoles,
      Map.empty
    )
    // no bids were called yet -> all players may call a win:
    assertEquals(result, PlayerPosition.All.map(_ -> Bid.Win).toMap)
  }

  test("in a regular game, the first card of the second trick, teams can still call 'no 90'") {
    val currentTrick = Trick(
      Player3,
      Map(
        Player3 -> (♣ | A),
        Player4 -> (♣ | Nine)
      )
    )
    val result = BidAnalyzer.nextPossibleBids(
      currentTrick,
      List(dummyFirstTrick),
      dummyRegularRoles,
      Map.empty
    )
    // no bids were called yet, but second card of second trick has been played -> all players may "no 90":
    assertEquals(result, PlayerPosition.All.map(_ -> BidExtension.No90).toMap)
  }

  test("once a team mate called a bit, the next possible bit must be higher") {
    val currentTrick = Trick(
      Player2,
      Map.empty
    )
    val existingBids: Map[PlayerPosition, Bid] = Map(Player3 -> Bid.Win)
    val result =
      BidAnalyzer.nextPossibleBids(currentTrick, List.empty, dummyRegularRoles, existingBids)

    // a member of the kontra-team already called kontra (-win), next bid for the team is "no 90":
    val expected = Map[PlayerPosition, Bid](
      Player1 -> Bid.Win,
      Player2 -> Bid.Win,
      Player3 -> BidExtension.No90,
      Player4 -> BidExtension.No90
    )
    assertEquals(result, expected)
  }
}

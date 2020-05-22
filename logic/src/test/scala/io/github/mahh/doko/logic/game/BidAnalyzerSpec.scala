package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.bids.WinningBid
import io.github.mahh.doko.shared.bids.WinningBid.BidExtension
import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition.Player2
import io.github.mahh.doko.shared.player.PlayerPosition.Player3
import io.github.mahh.doko.shared.player.PlayerPosition.Player4
import io.github.mahh.doko.shared.player.PlayerPosition._
import org.scalatest.funsuite.AnyFunSuite

class BidAnalyzerSpec extends AnyFunSuite {

  import BidAnalyzerSpec._

  test("in a regular game, teams can call win until (including) the first card of the second trick") {
    val currentTrick = Trick(
      Player3,
      Map(
        Player3 -> (♣ | A)
      )
    )
    val result = BidAnalyzer.nextPossibleBids(currentTrick, List(dummyFirstTrick), dummyRegularRoles, Map.empty)
    // no bids were called yet -> all players may call a win:
    assert(result === PlayerPosition.All.map(_ -> WinningBid.Win).toMap)
  }

  test("in a regular game, the first card of the second trick, teams can still call 'no 90'") {
    val currentTrick = Trick(
      Player3,
      Map(
        Player3 -> (♣ | A),
        Player4 -> (♣ | Nine)
      )
    )
    val result = BidAnalyzer.nextPossibleBids(currentTrick, List(dummyFirstTrick), dummyRegularRoles, Map.empty)
    // no bids were called yet, but second card of second trick has been played -> all players may "no 90":
    assert(result === PlayerPosition.All.map(_ -> BidExtension.No90).toMap)
  }

  test("once a team mate called a bit, the next possible bit must be higher") {
    val currentTrick = Trick(
      Player2,
      Map.empty
    )
    val existingBids: Map[PlayerPosition, WinningBid] = Map(Player3 -> WinningBid(None))
    val result = BidAnalyzer.nextPossibleBids(currentTrick, List.empty, dummyRegularRoles, existingBids)

    // a member of the kontra-team already called kontra (-win), next bid for the team is "no 90":
    val expected = Map(
      Player1 -> WinningBid.Win,
      Player2 -> WinningBid.Win,
      Player3 -> BidExtension.No90,
      Player4 -> BidExtension.No90
    )
    assert(result === expected)
  }
}

object BidAnalyzerSpec {

  private val dummyFirstTrick: (PlayerPosition.Player3.type, Trick) = Player3 -> Trick(
    Player1,
    Map(
      Player1 -> (♥ | A),
      Player2 -> (♥ | Nine),
      Player3 -> (♦ | A),
      Player4 -> (♥ | Nine)
    )
  )

  // roles of a regular game: 2 players are re, 2 are kontra:
  private val dummyRegularRoles: Map[PlayerPosition, Role] = Map(
    Player1 -> Role.Re,
    Player2 -> Role.Re,
    Player3 -> Role.Kontra,
    Player4 -> Role.Kontra
  )
}
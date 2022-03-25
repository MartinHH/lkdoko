package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.testutils.Checkers
import io.github.mahh.doko.shared.testutils.DeriveArbitrary.given
import minitest.SimpleTestSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

object TeamAnalyzerSpec extends SimpleTestSuite with Checkers {

  check("result of split teams contains all input players") {
    Prop.forAll { (players: TableMap[Role]) =>
      val (e, o) = TeamAnalyzer.splitTeams(players)
      e ++ o ?= players.keySet
    }
  }

  check("no player is member of both teams") {
    Prop.forAll { (players: TableMap[Role]) =>
      val (e, o) = TeamAnalyzer.splitTeams(players)
      e intersect o ?= Set.empty
    }
  }

  check("result of split teams and split teams with bids do the same splitting") {
    Prop.forAll { (players: TableMap[Role], bids: Map[PlayerPosition, Bid]) =>
      val (e1, o1) = TeamAnalyzer.splitTeams(players)
      val ((e2, _), (o2, _)) = TeamAnalyzer.splitTeamsWithBids(players, bids)
      (e1 ?= e2) :| "elders" && (o1 ?= o2) :| "others"
    }
  }

}

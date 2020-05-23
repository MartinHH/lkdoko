package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class TeamAnalyzerSpec extends AnyFunSuite with Checkers {
  import io.github.mahh.doko.logic.testutils.DeriveArbitrary._

  test("result of split teams contains all input players") {
    Prop.forAll { players: Map[PlayerPosition, Role] =>
      val (e, o) = TeamAnalyzer.splitTeams(players)
      e ++ o ?= players.keySet
    }
  }

  test("no player is member of both teams") {
    Prop.forAll { players: Map[PlayerPosition, Role] =>
      val (e, o) = TeamAnalyzer.splitTeams(players)
      e intersect o ?= Set.empty
    }
  }
}

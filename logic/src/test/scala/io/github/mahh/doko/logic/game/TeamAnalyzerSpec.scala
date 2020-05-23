package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.testutils.CheckersMinHundred
import io.github.mahh.doko.shared.player.PlayerPosition
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalatest.funsuite.AnyFunSuite

class TeamAnalyzerSpec extends AnyFunSuite with CheckersMinHundred {

  import io.github.mahh.doko.logic.testutils.DeriveArbitrary._

  test("result of split teams contains all input players") {
    check {
      Prop.forAll { players: Map[PlayerPosition, Role] =>
        val (e, o) = TeamAnalyzer.splitTeams(players)
        e ++ o ?= players.keySet
      }
    }
  }

  test("no player is member of both teams") {
    check {
      Prop.forAll { players: Map[PlayerPosition, Role] =>
        val (e, o) = TeamAnalyzer.splitTeams(players)
        e intersect o ?= Set.empty
      }
    }
  }

}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Playing
import io.github.mahh.doko.logic.game.FullGameState.Playing.PlayerState
import io.github.mahh.doko.logic.game.RuleConformingGens._
import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.deck.Rank._
import io.github.mahh.doko.shared.deck.Suit._
import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Trick
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgeTrickResult
import io.github.mahh.doko.shared.player.PlayerAction.PlayCard
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.player.PlayerPosition._
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.testutils.DeriveArbitrary.given
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Prop.propBoolean

object PlayingSpec extends AbstractFullGameStateSpec[Playing](playingMidGame()) {

  private def playableCards(state: Playing): Seq[(PlayerPosition, Card)] = {
    state.playerStates.toSeq.flatMap { case (pos, s) => s.canPlay.map(pos -> _) }
  }

  private val genValidPlay: Gen[(Playing, (PlayerPosition, PlayCard))] =
    for {
      state <- defaultGen
      cards = playableCards(state)
      if cards.nonEmpty
      (pos, card) <- Gen.oneOf(cards)
    } yield (state, pos -> PlayCard(card))

  private val genInvalidPlay: Gen[(Playing, (PlayerPosition, PlayCard))] =
    for {
      state <- defaultGen
      pos <- arbitrary[PlayerPosition]
      card <- arbitrary[Card]
      if !playableCards(state).contains(pos -> card)
    } yield (state, pos -> PlayerAction.PlayCard(card))

  check("after all cards are played and last trick is acknowledged, state transitions to RoundResults") {
    Prop.forAll(playingAfterAllCardsHaveBeenPlayedAndAcknowledged()) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.RoundResults]) :| s"stateOpt: $stateOpt"
    }
  }

  check("after less than all cards are played and possibly acknowledged, state is still Playing") {
    Prop.forAll(playingAfterLessThanAllCardsHaveBeenPlayed()) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.Playing]) :| s"stateOpt: $stateOpt"
    }
  }

  checkProp("while a trick is being played, exactly one player can play a card") { state =>
    state.finishedTrickOpt.isEmpty ==>
      (state.playerStates.values.count(_.canPlay.nonEmpty) ?= 1)
  }

  check("playable cards are always accepted") {
    Prop.forAll(genValidPlay) { case (state, action) =>
      state.handleAction.isDefinedAt(action)
    }
  }

  check("non-playable cards are never accepted") {
    Prop.forAll(genInvalidPlay) { case (state, action) =>
      !state.handleAction.isDefinedAt(action)
    }
  }

  check("playing a valid card leads to expected follow-up-state") {
    Prop.forAll(genValidPlay) { case (state, action@(player, PlayCard(card))) =>
      val followUp = state.handleAction.lift(action)
      val result: Prop =
        followUp match {
          case Some(p: Playing) if state.currentTrick.cards.size <= 2 =>
            (p.currentTrick.currentPlayer ?= Some(PlayerPosition.next(player))) :| "next player" &&
              (p.currentTrick.cards ?= (state.currentTrick.cards + (player -> card)))
          case Some(p: Playing) =>
            p.finishedTrickOpt.exists { t =>
              (t.trick.cards(player) == card) && (t.missingAcks == PlayerPosition.AllAsSet)
            }
          case _ =>
            Prop.falsified
        }
      result :| s"followUp = $followUp"
    }
  }

  test("in case of 'marriage', if another player wins the first trick, she marries the marriage player") {
    val rules = Rules(DeckRule.WithNines)
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
        TotalScores(List()),
        rules
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

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game
import io.github.mahh.doko.logic.game.RuleConformingGens.*
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.rules.Trumps
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.testutils.GenUtils
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Prop.propBoolean

class PovertyExchangeSpec extends AbstractFullGameStateSpec(povertyExchangeGen) {

  import PovertyExchangeSpec.*

  checkProp(
    "Number of cards in accepting playerState must be cardsPerPlayer + sizeOfPoverty"
  ) { state =>
    import state.rules.deckRule.cardsPerPlayer
    val playerState = state.playerStates(state.acceptingPlayer)
    val nCards = playerState.hand.size
    nCards ?= (cardsPerPlayer + playerState.sizeOfPoverty)
  }

  checkProp("Initially, selection is empty") { state =>
    (state.playerStates.map(_.playerState.map(_.selected)) ?= TableMap.fill(Some(Seq.empty))) &&
    state.selected.isEmpty && state
      .playerStates(state.acceptingPlayer)
      .playerState
      .map(_.selected)
      .contains(Seq.empty)
  }

  property("Selection is possible while selection.size is < sizeOfPoverty") {
    Prop.forAll(genWithNOfAcceptingHand(defaultGen)(_.sizeOfPoverty)) { case (state, cards) =>
      state.canApplyActionsProp(
        cards.map(card => state.acceptingPlayer -> PlayerAction.PovertySelect(card)): _*
      )
    }
  }

  property("PovertySelect adds to selected") {
    val gen: Gen[(FullGameState.PovertyExchange, Seq[Card])] =
      genWithNOfAcceptingHand(defaultGen)(_ => 1)
        .suchThat { case (s, _) => s.playerStates(s.acceptingPlayer).sizeOfPoverty > 0 }
    Prop.forAllNoShrink(gen) { case (state, cards) =>
      val card = cards.head
      state.handleAction.lift(state.acceptingPlayer -> PlayerAction.PovertySelect(card)) match {
        case Some(px: FullGameState.PovertyExchange) =>
          px.playerStates(px.acceptingPlayer).playerState.map(_.selected) ?= Some(Seq(card))
        case x =>
          Prop.falsified :| s"newState=$x"
      }
    }
  }

  property("Selection is not possible when selection.size is == sizeOfPoverty") {
    Prop.forAll(genWithNOfAcceptingHand(defaultGen)(_.sizeOfPoverty + 1)) { case (state, cards) =>
      val selection = cards.tail
      val selectionComplete = state.applyActions(
        selection.map(card => state.acceptingPlayer -> PlayerAction.PovertySelect(card)): _*
      )
      !selectionComplete.handleAction.isDefinedAt(
        state.acceptingPlayer -> PlayerAction.PovertySelect(cards.head)
      )
    }
  }

  checkProp("Deselection is not possible while selection is empty") { state =>
    val cardsToTry =
      state.playerStates(state.acceptingPlayer).playerState.toList.flatMap(_.selected)
    Prop.all(
      cardsToTry.map { c =>
        !state.handleAction
          .isDefinedAt(state.acceptingPlayer, PlayerAction.PovertyDeselect(c)) :| s"card=$c"
      }: _*
    )
  }

  property("Deselection is possible while selection is non-empty") {
    Prop.forAll(genWithNOfAcceptingHand(defaultGen)(_.sizeOfPoverty)) { case (state, cards) =>
      val selectionComplete = state.applyActions(
        cards.map(card => state.acceptingPlayer -> PlayerAction.PovertySelect(card)): _*
      )
      selectionComplete.canApplyActionsProp(
        cards.map(card => state.acceptingPlayer -> PlayerAction.PovertyDeselect(card)): _*
      )
    }
  }

  property("Returning the poverty is not possible while selection.size is < sizeOfPoverty") {
    Prop.forAll(genWithNOfAcceptingHand(defaultGen)(_.sizeOfPoverty - 1)) { case (state, cards) =>
      val (prop, _) = cards.foldLeft[(Prop, FullGameState)](Prop.passed -> state) {
        case ((p, s), c) =>
          (p && !s.handleAction.isDefinedAt(state.acceptingPlayer -> PlayerAction.PovertyReturn)) ->
            s.applyActions(state.acceptingPlayer -> PlayerAction.PovertySelect(c))
      }
      prop
    }
  }

  checkProp("Numbers of cards of non-involved playerStates must be cardsPerPlayer") { state =>
    import state.rules.deckRule.cardsPerPlayer
    Prop.all(
      PlayerPosition.All
        .filterNot(p => p == state.acceptingPlayer || p == state.poorPlayer)
        .map { pos =>
          val nCards = state.playerStates(pos).hand.size
          (nCards ?= cardsPerPlayer) :| s"pos=$pos"
        }: _*
    )
  }

  checkProp("Total number of cards in playerStates must be 4 * cardsPerPlayer") { state =>
    import state.rules.deckRule.cardsPerPlayer
    state.playerStates.values.map(_.hand.size).sum ?= (cardsPerPlayer * 4)
  }

  checkProp("Poor playerState must not have trumps") { state =>
    state.playerStates(state.poorPlayer).hand.forall(c => !Trumps.Default.isTrump(c))
  }

  property("If the accepting player returns sizeOfPoverty cards, state transitions to Playing") {
    Prop.forAll(povertyExchangeFollowUpGen) { stateOpt =>
      stateOpt.exists(_.isInstanceOf[FullGameState.Playing]) :| s"stateOpt: $stateOpt"
    }
  }

}

object PovertyExchangeSpec {
  private def genNOfHand(state: GameState.PovertyExchange, n: Int): Gen[Seq[Card]] = {
    val targetSize = state.hand.size - n
    GenUtils.takeSomeUntil(state.hand)(_.size <= targetSize)
  }

  private def genWithNOfAcceptingHand(
    genState: Gen[FullGameState.PovertyExchange]
  )(
    n: GameState.PovertyExchange => Int
  ): Gen[(FullGameState.PovertyExchange, Seq[Card])] = {
    for {
      s <- genState
      playerState = s.playerStates(s.acceptingPlayer)
      taken <- genNOfHand(playerState, n(playerState))
    } yield s -> taken
  }
}

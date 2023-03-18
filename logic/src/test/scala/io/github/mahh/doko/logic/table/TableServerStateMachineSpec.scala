package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.game.FullGameState
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.game.RuleConformingGens
import io.github.mahh.doko.logic.game.RuleConformingGens.rulesArb
import io.github.mahh.doko.logic.table.IncomingAction.ClientJoined
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.IncomingAction.ClientLeft
import io.github.mahh.doko.logic.table.IncomingAction.IncomingMessage
import io.github.mahh.doko.logic.table.TableServerStateMachine.TransitionResult
import io.github.mahh.doko.logic.table.client.ClientId
import io.github.mahh.doko.logic.table.client.given_Arbitrary_ClientId
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.logic.table.participant.given_Arbitrary_ParticipantId
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.AskingForReservations
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToServer.*
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.testutils.DeriveArbitrary.derived
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators

class TableServerStateMachineSpec extends ScalaCheckSuite:

  import TableServerStateMachineSpec.*
  import TableServerStateMachineSpec.given
  import TableServerStateMachine.transition

  property("Four players joining do not change the TableState") {
    Prop.forAll(genNDistinctClientJoined(4), rulesArb.arbitrary) { (joins, rules) =>
      given Rules = rules
      val initial = TableServerState.apply[ClientId]
      val result = joins.foldLeft(initial) { (state, join) =>
        transition(state, join).state
      }
      result.tableState =? initial.tableState
    }
  }

  property("While table is incomplete, joining does not lead to GameStateMessage") {
    Prop.forAll(genNDistinctClientJoined(3), rulesArb.arbitrary) { (joins, rules) =>
      given Rules = rules
      val initial = TableServerState.apply[ClientId]
      val (_, prop) = joins.foldLeft(initial -> Prop.passed) { case ((state, prop), join) =>
        val transitionResult = transition(state, join)
        val gameStateMessages = transitionResult.outgoingMessages.collect {
          case task @ ClientMessageTask(_, _: GameStateMessage) => task
        }
        transitionResult.state -> (prop && (gameStateMessages =? Seq.empty))
      }
      prop
    }
  }

  property("fourth joining leads to four GameStateMessages") {
    Prop.forAll(genNDistinctClientJoined(4), rulesArb.arbitrary) { (joins, rules) =>
      given Rules = rules
      val initial = TransitionResult.initial[ClientId]
      val result = joins.foldLeft(initial) { (tr, join) =>
        transition(tr.state, join)
      }
      val gameStateMessages = result.outgoingMessages.collect {
        case task @ ClientMessageTask(_, GameStateMessage(_)) => task
      }
      gameStateMessages.map(_.clientRef).toSet =? joins.map(_.clientId)
    }
  }

  property("fifth participant joining only leads to spectator-messages") {
    val genStateAndJoining = for {
      state <- genFourPlayersTableServerState
      c = state.clients
      joining <- arbitrary[ClientJoined[ClientId]].suchThat(j =>
        !c.byParticipant.contains(j.participantId) && !c.allReceivers.contains(j.clientId)
      )
    } yield state -> joining
    Prop.forAll(genStateAndJoining) { case (state, joining) =>
      val result = transition(state, joining)
      (result.state.tableState =? state.tableState) && {
        val messagesProps =
          result.outgoingMessages.map {
            case ClientMessageTask(c, GameStateMessage(gameState)) =>
              (c ?= joining.clientId) && (gameState.playerState =? None)
            case ClientMessageTask(c, _) =>
              c ?= joining.clientId
          }
        Prop.all(messagesProps: _*)
      }
    }
  }

  property("spectator leaving does not lead to any messages") {
    val genStateAndJoining = for {
      state <- genFourPlayersTableServerState
      c = state.clients
      joining <- arbitrary[ClientJoined[ClientId]].suchThat(j =>
        !c.byParticipant.contains(j.participantId) && !c.allReceivers.contains(j.clientId)
      )
    } yield state -> joining
    Prop.forAll(genStateAndJoining) { case (state, joining) =>
      val result = transition(
        transition(state, joining).state,
        ClientLeft(joining.clientId, joining.participantId)
      )
      // also: state should be same as before joining (if nothing else happened in between)
      (result.state =? state) && (result.outgoingMessages =? Seq.empty)
    }
  }

  property("one player leaving leads to three PlayersOnPauseMessages") {
    val genStateAndLeft: Gen[(TableServerState[ClientId], ClientLeft[ClientId], PlayerPosition)] =
      for {
        state <- genFourPlayersTableServerState
        playerIds <- Gen.oneOf(state.clients.byParticipant.toSeq)
        (pId, (pos, cIds)) = playerIds
      } yield (state, ClientLeft(cIds.head, pId), pos)
    Prop.forAll(genStateAndLeft) { case (state, left, pos) =>
      val result = transition(state, left)
      val expectedMessages = {
        val onPauseMsg = PlayersOnPauseMessage(Set(pos))
        (state.clients.allReceivers - left.clientId).map(ClientMessageTask(_, onPauseMsg))
      }
      (result.state.tableState.gameState =? state.tableState.gameState) &&
      (result.state.tableState.missingPlayers =? Set(pos)) &&
      (result.outgoingMessages.toSet =? expectedMessages)
    }
  }

  property("unknown client leaving only leads to logging") {
    val genStateAndUnknownClient: Gen[(TableServerState[ClientId], ClientId, ParticipantId)] =
      for {
        ts <- arbitrary[TableServerState[ClientId]]
        cid <- arbitrary[ClientId].suchThat(id => !ts.clients.allReceivers(id))
        pid <- arbitrary[ParticipantId].suchThat(id => !ts.clients.byParticipant.contains(id))
      } yield (ts, cid, pid)

    Prop.forAll(genStateAndUnknownClient) { case (state, clientId, participantId) =>
      val result =
        transition(state, ClientLeft(clientId, participantId))
      (result.state =? state) && (result.outgoingMessages =? Seq.empty)
    }
  }

  property("unknown client changing name only leads to logging") {
    val genStateAndUnknownClient: Gen[(TableServerState[ClientId], ParticipantId)] =
      for {
        ts <- arbitrary[TableServerState[ClientId]]
        pid <- arbitrary[ParticipantId].suchThat(id => !ts.clients.byParticipant.contains(id))
      } yield (ts, pid)

    Prop.forAll(genStateAndUnknownClient, arbitrary[String]) { case ((state, pId), name) =>
      val msg = IncomingMessage[ClientId](pId, SetUserName(name))
      val result = transition(state, msg)
      (result.state =? state) && (result.outgoingMessages =? Seq.empty)
    }
  }

  property(
    "when players are present, valid PlayerAction leads to new state (and GameStateMessages)"
  ) {
    val genStateAndAction
      : Gen[(TableServerState[ClientId], PlayerPosition, PlayerAction[GameState])] =
      for {
        state <- genFourPlayersTableServerState
        posAndAction <- RuleConformingGens.genValidAction(state.tableState.gameState)
        (pos, action) = posAndAction
      } yield (state, pos, action)
    Prop.forAll(genStateAndAction) { case (state, position, action) =>
      val pId = state.clients.byParticipant.collectFirst { case (pId, (`position`, _)) =>
        pId
      }.get // .get is safe because of genFourPlayersTableServerState
      val msg = IncomingMessage[ClientId](pId, PlayerActionMessage(action))
      val result = transition(state, msg)
      // apply on partial function is safe because of genValidAction
      val expectedGameState = state.tableState.gameState.handleAction(position -> action)
      (result.state.tableState.gameState =? expectedGameState) &&
      // without reproducing the logic under test, we cannot be more specific with our expectation for GameStateMessages
      // than "there must be at least one" (because there are states where the follow-up states only changers for one
      // player):
      result.outgoingMessages.collect { case c @ ClientMessageTask(_, _: GameStateMessage) =>
        c
      }.nonEmpty
    }
  }

object TableServerStateMachineSpec:

  private def genNDistinctClientJoined(n: Int): Gen[Set[ClientJoined[ClientId]]] =
    for {
      clientIds <- Gen.containerOfN[Set, ClientId](n, Arbitrary.arbitrary)
      participantIds <- Gen.containerOfN[Set, ParticipantId](n, Arbitrary.arbitrary)
    } yield clientIds.zip(participantIds).map(ClientJoined.apply[ClientId].tupled)

  private val genFourPlayersTableClients: Gen[TableClients[ClientId]] =
    for {
      joins <- genNDistinctClientJoined(PlayerPosition.All.size)
    } yield
      val byParticipant = PlayerPosition.All
        .zip(joins)
        .map { case (pos, join) =>
          join.participantId -> (pos -> Set(join.clientId))
        }
        .toMap
      TableClients(byParticipant, Set.empty)

  private given Arbitrary[FullGameState] =
    // for these tests, we could be more arbitrary than using rule-conforming FullGameState-
    // instances only, but currently, there is no truly arbitrary instance and fully deriving
    // one seems to exceed the compilers capabilities.
    Arbitrary(RuleConformingGens.fullGameStateGen)

  private given Arbitrary[TableClients[ClientId]] = derived

  private given Arbitrary[TableServerState[ClientId]] = derived

  /**
   * Generates a TableServerState with 4 players (one client each) and no spectators.
   */
  private val genFourPlayersTableServerState: Gen[TableServerState[ClientId]] =
    for {
      tc <- genFourPlayersTableClients
      gs <- RuleConformingGens.fullGameStateGen
      ns <- arbitrary[Map[PlayerPosition, String]]
    } yield TableServerState(tc, FullTableState(ns, gs, Set.empty))

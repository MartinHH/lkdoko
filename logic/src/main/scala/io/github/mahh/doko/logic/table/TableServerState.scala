package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.TableServerState.StateAndOutput
import io.github.mahh.doko.logic.table.TableServerError.ActionNotApplicable
import io.github.mahh.doko.logic.table.TableServerError.ClientLeftError
import io.github.mahh.doko.logic.table.TableServerError.NonExistingPlayer
import io.github.mahh.doko.logic.table.TableServerError.PlayerActionError
import io.github.mahh.doko.logic.table.TableServerError.PlayersIncomplete
import io.github.mahh.doko.logic.table.TableServerError.UnknownClient
import io.github.mahh.doko.logic.table.TableServerState.TransitionEither
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TableIsFull
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.utils.toEither

import scala.collection.immutable

case class TableServerState[Ref](
  clients: TableClients[Ref],
  tableState: FullTableState
) {

  private def toAll(msg: MessageToClient): Vector[ClientMessageTask[Ref]] = {
    clients.allReceivers.toVector.map(ClientMessageTask(_, msg))
  }

  private def fullStateMessageTasks(
    clientRef: Ref,
    posOpt: Option[PlayerPosition]
  ): Vector[ClientMessageTask[Ref]] = {
    def tell[A](
      getA: FullTableState => A,
      msgFactory: A => MessageToClient
    ): ClientMessageTask[Ref] = {
      val msg = msgFactory(getA(tableState))
      ClientMessageTask(clientRef, msg)
    }

    def gameState(ts: FullTableState): GameState =
      posOpt.flatMap(ts.playerStates.get).getOrElse(ts.gameState.spectatorState)

    def tableFull =
      if (posOpt.isEmpty) Vector(ClientMessageTask(clientRef, TableIsFull)) else Vector.empty
    Vector(
      tell(_.playerNames, PlayersMessage.apply),
      tell(_.totalScores, TotalScoresMessage.apply),
      tell(_.missingPlayers, PlayersOnPauseMessage.apply),
      tell(gameState, GameStateMessage.apply)
    ) ++ tableFull

  }

  private def joiningMessageTasks(clientRef: Ref): Vector[ClientMessageTask[Ref]] = {
    Vector(
      MessageToClient.Joining,
      PlayersMessage(tableState.playerNames)
    ).map(ClientMessageTask(clientRef, _))
  }

  private def welcomeMessageTasks(
    clientRef: Ref,
    posOpt: Option[PlayerPosition]
  ): Vector[ClientMessageTask[Ref]] = {
    if (clients.isComplete) {
      fullStateMessageTasks(clientRef, posOpt)
    } else {
      joiningMessageTasks(clientRef)
    }
  }

  private def updatedGameStateAndMessageTasks(
    newTableState: FullTableState,
    force: Boolean = false
  ): StateAndOutput[Ref] = {

    def tellAllIfChanged[A](
      getA: FullTableState => A,
      msgFactory: A => MessageToClient
    ): Vector[ClientMessageTask[Ref]] = {
      val newA = getA(newTableState)
      if (getA(tableState) != newA || force) {
        val msg = msgFactory(newA)
        toAll(msg)
      } else {
        Vector.empty
      }
    }

    val gameStateMessagesForPlayers: immutable.Iterable[ClientMessageTask[Ref]] =
      for {
        (pos, ps) <- newTableState.playerStates
        if force || !tableState.playerStates.get(pos).contains(ps)
        (_, clientIds) <- clients.players.get(pos).toList
        client <- clientIds
      } yield {
        ClientMessageTask(client, GameStateMessage(ps))
      }

    val gameStateMessagesForSpectators: immutable.Iterable[ClientMessageTask[Ref]] =
      if (
        clients.spectators.nonEmpty &&
        (force || tableState.gameState.spectatorState != newTableState.gameState.spectatorState)
      ) {
        val spectatorState = newTableState.gameState.spectatorState
        clients.spectators.map(ClientMessageTask(_, GameStateMessage(spectatorState)))
      } else {
        Vector.empty
      }
    val allMessageTasks =
      tellAllIfChanged(_.playerNames, PlayersMessage.apply) ++
        tellAllIfChanged(_.totalScores, TotalScoresMessage.apply) ++
        tellAllIfChanged(_.missingPlayers, PlayersOnPauseMessage.apply) ++
        gameStateMessagesForPlayers ++
        gameStateMessagesForSpectators

    copy(tableState = newTableState) -> allMessageTasks
  }

  private def withPlayer(
    id: ParticipantId,
    actorRef: Ref,
    pos: PlayerPosition
  ): TableServerState[Ref] = {
    copy(clients = clients.withPlayer(id, actorRef, pos))
  }

  private def withoutReceiver(
    id: ParticipantId,
    clientRef: Ref
  ): TableServerState[Ref] = {
    copy(clients = clients.withoutReceiver(id, clientRef))
  }

  private def withSpectator(
    clientRef: Ref
  ): TableServerState[Ref] = {
    copy(clients = clients.copy(spectators = clients.spectators + clientRef))
  }

  private def withoutSpectator(clientRef: Ref): TableServerState[Ref] = {
    copy(clients = clients.copy(spectators = clients.spectators - clientRef))
  }

  private[table] def applyClientJoined(
    participantId: ParticipantId,
    ref: Ref
  ): StateAndOutput[Ref] =
    if (clients.byParticipant.contains(participantId)) {
      // new / rejoined client for existing player:
      val (pos, _) = clients.byParticipant(participantId)
      val newGameState = tableState.playerRejoins(pos)
      val (newState, msgTasks) =
        withPlayer(participantId, ref, pos)
          .updatedGameStateAndMessageTasks(newGameState)
      // add welcomeMessageTasks to ensure the client has the latest state
      newState -> (msgTasks ++ newState.welcomeMessageTasks(ref, Some(pos)))
    } else {
      val newPosOpt = PlayerPosition.All.find(p => !clients.players.contains(p))
      newPosOpt.fold[StateAndOutput[Ref]] {
        // table is full already - join as spectator:
        withSpectator(ref) -> welcomeMessageTasks(ref, posOpt = None)
      } { pos =>
        // add new player...
        val stateWithPlayer = withPlayer(participantId, ref, pos)
        if (stateWithPlayer.clients.isComplete) {
          // reveal the initial game state:
          stateWithPlayer.updatedGameStateAndMessageTasks(
            stateWithPlayer.tableState,
            force = true
          )
        } else {
          stateWithPlayer -> stateWithPlayer.joiningMessageTasks(ref)
        }
      }
    }

  private[table] def applyClientLeft(
    participantId: ParticipantId,
    ref: Ref
  ): TransitionEither[ClientLeftError, Ref] =
    if (clients.byParticipant.contains(participantId)) {
      val (pos, _) = clients.byParticipant(participantId)
      val stateWithoutReceiver = withoutReceiver(participantId, ref)
      val playerHasRemainingClient =
        stateWithoutReceiver.clients.players.get(pos).exists { case (_, cIds) => cIds.nonEmpty }
      if (playerHasRemainingClient) {
        Right(stateWithoutReceiver, Vector.empty)
      } else {
        val stateWithPaused = stateWithoutReceiver.tableState.playerPauses(pos)
        Right(stateWithoutReceiver.updatedGameStateAndMessageTasks(stateWithPaused))
      }
    } else if (clients.spectators.contains(ref)) {
      Right(withoutSpectator(ref) -> Vector.empty)
    } else {
      Left(UnknownClient)
    }

  private[table] def applyPlayerAction(
    playerId: ParticipantId,
    action: PlayerAction[GameState]
  ): TransitionEither[PlayerActionError, Ref] =
    if (!clients.isComplete) {
      Left(PlayersIncomplete)
    } else {
      for {
        pos <- clients.posForParticipant(playerId).toEither(NonExistingPlayer)
        gs <- tableState.handleAction(pos, action).toEither(ActionNotApplicable)
      } yield updatedGameStateAndMessageTasks(gs)
    }

  private[table] def applyUserNameChange(
    playerId: ParticipantId,
    name: String
  ): TransitionEither[NonExistingPlayer.type, Ref] =
    clients.posForParticipant(playerId).toEither(NonExistingPlayer).map { pos =>
      val newTableState = tableState.withUpdatedUserName(pos, name)
      updatedGameStateAndMessageTasks(newTableState)
    }
}

object TableServerState {
  def apply[Ref](using rules: Rules): TableServerState[Ref] =
    TableServerState(TableClients.empty[Ref], FullTableState.apply)

  type StateAndOutput[Ref] = (TableServerState[Ref], Vector[ClientMessageTask[Ref]])

  type TransitionEither[E <: TableServerError, Ref] = Either[E, StateAndOutput[Ref]]

}

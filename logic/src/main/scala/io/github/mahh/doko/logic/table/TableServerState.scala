package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.ActionNotApplicable
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.ClientLeftError
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.NonExistingPlayer
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.PlayerActionError
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.PlayersIncomplete
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.UnknownClient
import io.github.mahh.doko.logic.table.TableServerState.TransitionOutput
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
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
    clientRefs: Set[Ref],
    posOpt: Option[PlayerPosition]
  ): Vector[ClientMessageTask[Ref]] = {
    def tell[A](
      getA: FullTableState => A,
      msgFactory: A => MessageToClient
    ): Set[ClientMessageTask[Ref]] = {
      val msg = msgFactory(getA(tableState))
      clientRefs.map(ClientMessageTask(_, msg))
    }

    tell(_.playerNames, PlayersMessage.apply).toVector ++
      tell(_.totalScores, TotalScoresMessage.apply) ++
      tell(_.missingPlayers, PlayersOnPauseMessage.apply) ++ {
        val gameState =
          posOpt.flatMap(tableState.playerStates.get).getOrElse(tableState.gameState.spectatorState)
        clientRefs.map(ClientMessageTask(_, GameStateMessage(gameState)))
      }
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
      fullStateMessageTasks(Set(clientRef), posOpt)
    } else {
      joiningMessageTasks(clientRef)
    }
  }

  private def updatedGameStateAndMessageTasks(
    newTableState: FullTableState,
    force: Boolean = false
  ): TransitionOutput[Ref] = {

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
        clients <- clients.byPos.get(pos).toList
        client <- clients
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

  def applyClientJoined(
    clientId: ParticipantId,
    ref: Ref
  ): TransitionOutput[Ref] =
    if (clients.byParticipant.contains(clientId)) {
      // new / rejoined client for existing player:
      val (pos, _) = clients.byParticipant(clientId)
      val newGameState = tableState.playerRejoins(pos)
      val (newState, msgTasks) =
        withPlayer(clientId, ref, pos)
          .updatedGameStateAndMessageTasks(newGameState)
      // add welcomeMessageTasks to ensure the client has the latest state
      newState -> (msgTasks ++ newState.welcomeMessageTasks(ref, Some(pos)))
    } else {
      val newPosOpt = PlayerPosition.All.find(p => !clients.byPos.contains(p))
      newPosOpt.fold[TransitionOutput[Ref]] {
        // table is full already - join as spectator:
        withSpectator(ref) -> welcomeMessageTasks(ref, posOpt = None)
      } { pos =>
        // add new player...
        val stateWithPlayer = withPlayer(clientId, ref, pos)
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

  def applyClientLeft(
    clientId: ParticipantId,
    ref: Ref
  ): Either[ClientLeftError, TransitionOutput[Ref]] =
    if (clients.byParticipant.contains(clientId)) {
      val (pos, _) = clients.byParticipant(clientId)
      val stateWithoutReceiver = withoutReceiver(clientId, ref)
      if (stateWithoutReceiver.clients.byPos(pos).nonEmpty) {
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

  def applyPlayerAction(
    playerId: ParticipantId,
    action: PlayerAction[GameState]
  ): Either[PlayerActionError, TransitionOutput[Ref]] =
    if (!clients.isComplete) {
      Left(PlayersIncomplete)
    } else {
      for {
        pos <- clients.posForParticipant(playerId).toEither(NonExistingPlayer)
        gs <- tableState.handleAction(pos, action).toEither(ActionNotApplicable)
      } yield updatedGameStateAndMessageTasks(gs)
    }

  def applyUserNameChange(
    playerId: ParticipantId,
    name: String
  ): Either[NonExistingPlayer.type, TransitionOutput[Ref]] =
    clients.posForParticipant(playerId).toEither(NonExistingPlayer).map { pos =>
      val newTableState = tableState.withUpdatedUserName(pos, name)
      updatedGameStateAndMessageTasks(newTableState)
    }
}

object TableServerState {
  def apply[Ref](using rules: Rules): TableServerState[Ref] =
    TableServerState(TableClients.empty[Ref], FullTableState.apply)

  type TransitionOutput[Ref] = (TableServerState[Ref], Vector[ClientMessageTask[Ref]])

  object TransitionOutput {
    def initial[Ref](using rules: Rules): TransitionOutput[Ref] =
      TableServerState.apply[Ref] -> Vector.empty[ClientMessageTask[Ref]]
  }

  sealed trait TableServerError

  object TableServerError {

    sealed trait PlayerActionError extends TableServerError

    case object NonExistingPlayer extends PlayerActionError

    case object PlayersIncomplete extends PlayerActionError

    case object ActionNotApplicable extends PlayerActionError

    sealed trait ClientLeftError extends TableServerError

    case object UnknownClient extends ClientLeftError
  }
}

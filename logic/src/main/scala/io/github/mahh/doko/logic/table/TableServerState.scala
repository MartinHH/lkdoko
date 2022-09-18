package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.player.PlayerPosition

import java.util.UUID
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

  def joiningMessageTasks(clientRef: Ref): Vector[ClientMessageTask[Ref]] = {
    Vector(
      MessageToClient.Joining,
      PlayersMessage(tableState.playerNames)
    ).map(ClientMessageTask(clientRef, _))
  }

  def welcomeMessageTasks(
    clientRef: Ref,
    posOpt: Option[PlayerPosition]
  ): Vector[ClientMessageTask[Ref]] = {
    if (clients.isComplete) {
      fullStateMessageTasks(Set(clientRef), posOpt)
    } else {
      joiningMessageTasks(clientRef)
    }
  }

  def updatedGameStateAndMessageTasks(
    newTableState: FullTableState,
    force: Boolean = false
  ): (TableServerState[Ref], Vector[ClientMessageTask[Ref]]) = {

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

  def withPlayer(
    id: UUID,
    actorRef: Ref,
    pos: PlayerPosition
  ): TableServerState[Ref] = {
    copy(clients = clients.withPlayer(id, actorRef, pos))
  }

  def withoutReceiver(id: UUID, clientRef: Ref): TableServerState[Ref] = {
    copy(clients = clients.withoutReceiver(id, clientRef))
  }

  def withSpectator(
    clientRef: Ref
  ): TableServerState[Ref] = {
    copy(clients = clients.copy(spectators = clients.spectators + clientRef))
  }

  def withoutSpectator(clientRef: Ref): TableServerState[Ref] = {
    copy(clients = clients.copy(spectators = clients.spectators - clientRef))
  }
}

object TableServerState {
  def apply[Ref](using rules: Rules): TableServerState[Ref] =
    TableServerState(TableClients[Ref](), FullTableState.apply)
}

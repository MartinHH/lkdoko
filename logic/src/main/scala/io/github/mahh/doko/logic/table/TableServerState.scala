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

case class TableServerState[Ref](
  clients: TableClients[Ref],
  tableState: FullTableState
)(using c: Client[Ref]) {

  private def tellAll(msg: MessageToClient): Unit = {
    clients.allReceivers.foreach(_.tell(msg))
  }

  def tellFullStateTo(
    clientRefs: Set[Ref],
    posOpt: Option[PlayerPosition]
  ): Unit = {
    def tell[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
      val msg = msgFactory(getA(tableState))
      clientRefs.foreach(_.tell(msg))
    }

    tell(_.playerNames, PlayersMessage.apply)
    tell(_.totalScores, TotalScoresMessage.apply)
    tell(_.missingPlayers, PlayersOnPauseMessage.apply)

    val gameState =
      posOpt.flatMap(tableState.playerStates.get).getOrElse(tableState.gameState.spectatorState)
    clientRefs.foreach(_.tell(GameStateMessage(gameState)))
  }

  def tellJoiningMessages(clientRef: Ref): Unit = {
    clientRef.tell(MessageToClient.Joining)
    clientRef.tell(PlayersMessage(tableState.playerNames))
  }

  def tellWelcomeMessages(
    clientRef: Ref,
    posOpt: Option[PlayerPosition]
  ): Unit = {
    if (clients.isComplete) {
      tellFullStateTo(Set(clientRef), posOpt)
    } else {
      tellJoiningMessages(clientRef)
    }
  }

  def updateGameStateAndTellPlayers(
    newTableState: FullTableState,
    force: Boolean = false
  ): TableServerState[Ref] = {

    def tellAllIfChanged[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
      val newA = getA(newTableState)
      if (getA(tableState) != newA || force) {
        val msg = msgFactory(newA)
        tellAll(msg)
      }
    }

    tellAllIfChanged(_.playerNames, PlayersMessage.apply)
    tellAllIfChanged(_.totalScores, TotalScoresMessage.apply)
    tellAllIfChanged(_.missingPlayers, PlayersOnPauseMessage.apply)

    for {
      (pos, ps) <- newTableState.playerStates
      if force || !tableState.playerStates.get(pos).contains(ps)
      clients <- clients.byPos.get(pos)
      client <- clients
    } {
      client.tell(GameStateMessage(ps))
    }
    if (
      clients.spectators.nonEmpty &&
        (force || tableState.gameState.spectatorState != newTableState.gameState.spectatorState)
    ) {
      val spectatorState = newTableState.gameState.spectatorState
      clients.spectators.foreach(_.tell(GameStateMessage(spectatorState)))
    }

    copy(tableState = newTableState)
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
  def apply[Ref](using rules: Rules, c: Client[Ref]): TableServerState[Ref] =
    TableServerState(TableClients[Ref](), FullTableState.apply)
}
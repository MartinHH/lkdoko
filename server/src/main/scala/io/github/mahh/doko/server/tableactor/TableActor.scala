package io.github.mahh.doko.server.tableactor

import java.util.UUID

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.game.TableState
import io.github.mahh.doko.server.tableactor.IncomingAction.IncomingMessageFromClient
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerJoined
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerLeft
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import org.slf4j.Logger

/**
 * Actor holding (and updating) the state of one table.
 */
object TableActor {

  private case class Players(
    byUuid: Map[UUID, (ActorRef[OutgoingAction], PlayerPosition)] = Map.empty
  ) {
    val byPos: Map[PlayerPosition, ActorRef[OutgoingAction]] = byUuid.values.map {
      case (ref, pos) => pos -> ref
    }.toMap

    val isComplete: Boolean = byPos.size >= PlayerPosition.All.size

    def withPlayer(id: UUID, actorRef: ActorRef[OutgoingAction], pos: PlayerPosition): Players = {
      copy(byUuid + (id -> (actorRef, pos)))
    }
  }

  private case class State(
    players: Players = Players(),
    tableState: TableState = TableState()
  ) {

    def updateGameStateAndTellPlayers(newTableState: TableState, log: Logger): State = {
      if (tableState.playerNames != newTableState.playerNames) {
        val msg = OutgoingAction.NewMessageToClient(PlayersMessage(newTableState.playerNames))
        players.byPos.values.foreach(_ ! msg)
      }

      if (tableState.totalScores != newTableState.totalScores) {
        val msg = OutgoingAction.NewMessageToClient(TotalScoresMessage(newTableState.totalScores))
        players.byPos.values.foreach(_ ! msg)
      }


      for {
        (pos, ps) <- newTableState.playerStates
        actor <- players.byPos.get(pos)
        if !tableState.playerStates.get(pos).contains(ps)
      } {
        log.trace(s"Telling this to $pos: $ps")
        actor ! OutgoingAction.NewMessageToClient(GameStateMessage(ps))
      }
      copy(tableState = newTableState)
    }

    def withPlayer(id: UUID, actorRef: ActorRef[OutgoingAction], pos: PlayerPosition): State = {
      copy(players = players.withPlayer(id, actorRef, pos))
    }
  }

  def behavior: Behavior[IncomingAction] = behavior(State())

  private def behavior(state: State): Behavior[IncomingAction] = Behaviors.receive { (ctx, msg) =>
    ctx.log.trace(s"Received: $msg")
    msg match {
      case j: PlayerJoined if state.players.byUuid.contains(j.playerId) =>
        val (_, pos) = state.players.byUuid(j.playerId)
        val newGameState = state.tableState.playerRejoins(pos)
        val newState =
          state.withPlayer(j.playerId, j.replyTo, pos).updateGameStateAndTellPlayers(newGameState, ctx.log)
        j.replyTo ! OutgoingAction.NewMessageToClient(PlayersMessage(newGameState.playerNames))
        behavior(newState)
      case j: PlayerJoined if !state.players.isComplete =>
        val newPosAndTableState: Option[(PlayerPosition, TableState)] = for {
          pos <- PlayerPosition.All.find(p => !state.players.byPos.contains(p))
          gs <- state.tableState.handlePlayerAction.lift(pos -> PlayerAction.Join)
        } yield pos -> gs
        newPosAndTableState.fold {
          ctx.log.warn(s"Could not join even though not all positions are taken: $state")
          Behaviors.same[IncomingAction]
        } { case (pos, gs) =>
          behavior(state.withPlayer(j.playerId, j.replyTo, pos).updateGameStateAndTellPlayers(gs, ctx.log))
        }
      case j: PlayerJoined =>
        ctx.log.warn(s"Player tried to join when table as complete: $state")
        j.replyTo ! OutgoingAction.NewMessageToClient(MessageToClient.TableIsFull)
        j.replyTo ! OutgoingAction.Completed
        Behaviors.same
      case l: PlayerLeft if state.players.byUuid.contains(l.playerId) =>
        ctx.log.warn(s"Player ${l.playerId} left")
        val (_, pos) = state.players.byUuid(l.playerId)
        val newState = state.tableState.playerPauses(pos)
        behavior(state.updateGameStateAndTellPlayers(newState, ctx.log))
      case l: PlayerLeft =>
        ctx.log.debug(s"A player left that was not even playing: $l")
        Behaviors.same
      case a: IncomingMessageFromClient =>
        val newGameState = for {
          (_, pos) <- state.players.byUuid.get(a.playerId)
          gs <- state.tableState.handleMessage(pos, a.msg)
        } yield gs
        newGameState.fold {
          ctx.log.debug(s"Action not applicable to state: $a -> $state")
          Behaviors.same[IncomingAction]
        } { gs =>
          behavior(state.updateGameStateAndTellPlayers(gs, ctx.log))
        }

    }
  }


}

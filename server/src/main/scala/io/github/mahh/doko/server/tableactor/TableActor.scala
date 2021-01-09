package io.github.mahh.doko.server.tableactor

import java.util.UUID

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.server.tableactor.IncomingAction.IncomingMessageFromClient
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerJoined
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerLeft
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
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
    players: Players,
    tableState: FullTableState
  ) {

    def tellAll(msg: OutgoingAction): Unit = players.byPos.values.foreach(_ ! msg)

    def updateGameStateAndTellPlayers(
      newTableState: FullTableState,
      log: Logger,
      forceGameStateTelling: Boolean = false
    ): State = {

      def tellAllIfChanged[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
        val newA = getA(newTableState)
        if (getA(tableState) != newA) {
          val msg = OutgoingAction.NewMessageToClient(msgFactory(newA))
          tellAll(msg)
        }
      }

      tellAllIfChanged(_.playerNames, PlayersMessage.apply)

      tellAllIfChanged(_.totalScores, TotalScoresMessage.apply)

      tellAllIfChanged(_.missingPlayers, PlayersOnPauseMessage.apply)

      for {
        (pos, ps) <- newTableState.playerStates
        actor <- players.byPos.get(pos)
        if forceGameStateTelling || !tableState.playerStates.get(pos).contains(ps)
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

  def behavior(implicit rules: Rules): Behavior[IncomingAction] = behavior(State(rules = rules))

  // TODO: the "joining" logic probably should be moved into FullTableState (it does not depend on akka)

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
        val newPos: Option[PlayerPosition] =
          PlayerPosition.All.find(p => !state.players.byPos.contains(p))
        newPos.fold {
          // impossible to reach, but nevertheless:
          ctx.log.warn(s"Could not join even though not all positions are taken: $state")
          Behaviors.same[IncomingAction]
        } { pos =>
          val newState = state.withPlayer(j.playerId, j.replyTo, pos)
          if (newState.players.isComplete) {
            // reveal the initial game state:
            newState.updateGameStateAndTellPlayers(newState.tableState, ctx.log, forceGameStateTelling = true)
          } else {
            j.replyTo ! OutgoingAction.NewMessageToClient(MessageToClient.Joining)
          }
          behavior(newState)
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
      case IncomingMessageFromClient(id, SetUserName(name)) =>
        state.players.byUuid.get(id).fold {
          ctx.log.debug(s"Unknown user id, cannot rename: $id")
          Behaviors.same[IncomingAction]
        } { case (_, pos) =>
          val newTableState = state.tableState.withUpdatedUserName(pos, name)
          behavior(state.updateGameStateAndTellPlayers(newTableState, ctx.log))
        }
      case IncomingMessageFromClient(id, PlayerActionMessage(action)) if state.players.isComplete =>
        val newGameState = for {
          (_, pos) <- state.players.byUuid.get(id)
          gs <- state.tableState.handleAction(pos, action)
        } yield gs
        newGameState.fold {
          ctx.log.debug(s"Action not applicable to state: $action -> $state")
          Behaviors.same[IncomingAction]
        } { gs =>
          behavior(state.updateGameStateAndTellPlayers(gs, ctx.log))
        }
      case IncomingMessageFromClient(id, PlayerActionMessage(action)) =>
        ctx.log.debug(s"A player tried to trigger an action before table was completed ($id, $action)")
        Behaviors.same
      case l: PlayerLeft =>
        ctx.log.debug(s"A player left that was not even playing: $l")
        Behaviors.same
    }
  }

  private object State {
    def apply(implicit rules: Rules): State = State(Players(), FullTableState.apply)
  }

}

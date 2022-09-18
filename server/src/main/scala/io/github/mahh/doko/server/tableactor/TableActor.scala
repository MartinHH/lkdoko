package io.github.mahh.doko.server.tableactor

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.Client
import io.github.mahh.doko.logic.table.TableClients
import io.github.mahh.doko.logic.table.TableServerState
import io.github.mahh.doko.server.tableactor.IncomingAction.IncomingMessageFromClient
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerJoined
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerLeaving
import io.github.mahh.doko.server.tableactor.IncomingAction.PlayerReceiverDied
import io.github.mahh.doko.server.tableactor.IncomingAction.SpectatorReceiverDied
import io.github.mahh.doko.server.tableactor.OutgoingAction.NewMessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerPosition
import org.slf4j.Logger

import java.util.UUID

/**
 * Actor holding (and updating) the state of one table.
 */
object TableActor {

  given Client[ActorRef[OutgoingAction]] with
    extension (a: ActorRef[OutgoingAction])
      def tell(message: MessageToClient): Unit = a ! NewMessageToClient(message)

  def behavior(using rules: Rules): Behavior[IncomingAction] =
    behavior(TableServerState.apply)

  // TODO: the "joining" logic probably should be moved into FullTableState (it does not depend on akka)

  extension (state: TableServerState[ActorRef[OutgoingAction]])
    private def withPlayer(
      ctx: ActorContext[IncomingAction],
      j: PlayerJoined,
      pos: PlayerPosition
    ): TableServerState[ActorRef[OutgoingAction]] = {
      ctx.watchWith(j.replyTo, PlayerReceiverDied(j.playerId, pos, j.replyTo))
      state.withPlayer(j.playerId, j.replyTo, pos)
    }

  private def behavior(
    state: TableServerState[ActorRef[OutgoingAction]]
  ): Behavior[IncomingAction] =
    Behaviors.receive[IncomingAction] { (ctx, msg) =>
      ctx.log.trace(s"Received: $msg")
      msg match {
        case j: PlayerJoined if state.clients.byUuid.contains(j.playerId) =>
          val (pos, _) = state.clients.byUuid(j.playerId)
          val newGameState = state.tableState.playerRejoins(pos)
          val newState =
            state.withPlayer(ctx, j, pos).updateGameStateAndTellPlayers(newGameState)
          // make sure the player has the latest state (even if the browser was closed)
          newState.tellWelcomeMessages(j.replyTo, Some(pos))
          behavior(newState)
        case j: PlayerJoined if !state.clients.isComplete =>
          val newPos: Option[PlayerPosition] =
            PlayerPosition.All.find(p => !state.clients.byPos.contains(p))
          newPos.fold {
            // impossible to reach, but nevertheless:
            ctx.log.warn(s"Could not join even though not all positions are taken: $state")
            Behaviors.same[IncomingAction]
          } { pos =>
            val newState = state.withPlayer(ctx, j, pos)
            if (newState.clients.isComplete) {
              // reveal the initial game state:
              newState.updateGameStateAndTellPlayers(newState.tableState, force = true)
            } else {
              newState.tellJoiningMessages(j.replyTo)
            }
            behavior(newState)
          }
        case j: PlayerJoined =>
          ctx.log.info(s"Player tried to join when table as complete - joins as spectator")
          state.tellWelcomeMessages(j.replyTo, posOpt = None)
          ctx.watchWith(j.replyTo, SpectatorReceiverDied(j.replyTo))
          behavior(state.withSpectator(j.replyTo))
        case d: PlayerReceiverDied if state.clients.byUuid.contains(d.playerId) =>
          ctx.log.info(s"Player ${d.playerId} left (${d.pos}, ${d.receiver.path.name})")
          val stateWithoutReceiver = state.withoutReceiver(d.playerId, d.receiver)
          val (pos, _) = stateWithoutReceiver.clients.byUuid(d.playerId)
          if (stateWithoutReceiver.clients.byPos(pos).nonEmpty) {
            behavior(stateWithoutReceiver)
          } else {
            val newState = stateWithoutReceiver.tableState.playerPauses(pos)
            behavior(stateWithoutReceiver.updateGameStateAndTellPlayers(newState))
          }
        case d: SpectatorReceiverDied =>
          ctx.log.info(s"Spectator left (${d.receiver.path.name})")
          behavior(state.withoutSpectator(d.receiver))
        case IncomingMessageFromClient(id, SetUserName(name)) =>
          state.clients.byUuid
            .get(id)
            .fold {
              ctx.log.debug(s"Unknown user id, cannot rename: $id")
              Behaviors.same[IncomingAction]
            } { case (pos, _) =>
              val newTableState = state.tableState.withUpdatedUserName(pos, name)
              behavior(state.updateGameStateAndTellPlayers(newTableState))
            }
        case IncomingMessageFromClient(id, PlayerActionMessage(action))
            if state.clients.isComplete =>
          val newGameState = for {
            (pos, _) <- state.clients.byUuid.get(id)
            gs <- state.tableState.handleAction(pos, action)
          } yield gs
          newGameState.fold {
            ctx.log.debug(s"Action not applicable to state: $action -> $state")
            Behaviors.same[IncomingAction]
          } { gs =>
            behavior(state.updateGameStateAndTellPlayers(gs))
          }
        case IncomingMessageFromClient(id, PlayerActionMessage(action)) =>
          ctx.log.debug(
            s"A player tried to trigger an action before table was completed ($id, $action)"
          )
          Behaviors.same
        case l: PlayerLeaving =>
          // termination of incoming socket-stream is ignored - behavior reacts on corresponding
          // termination of outgoing socket-stream (on ReceiverDied)
          ctx.log.debug(s"Player ${l.playerId} leaving")
          Behaviors.same
        case d: PlayerReceiverDied =>
          ctx.log.debug(s"A player left that was not even playing: $d")
          Behaviors.same
      }
    }

}

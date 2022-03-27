package io.github.mahh.doko.server.tableactor

import java.util.UUID

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
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

/**
 * Actor holding (and updating) the state of one table.
 */
object TableActor {

  /**
   * Holds `ActorRef`s of all connected clients
   *
   * @param byUuid     The players by id.
   * @param spectators Spectators (not playing, but may see what is being played).
   */
  private case class Clients(
    byUuid: Map[UUID, (PlayerPosition, Set[ActorRef[OutgoingAction]])] = Map.empty,
    spectators: Set[ActorRef[OutgoingAction]] = Set.empty
  ) {
    val byPos: Map[PlayerPosition, Set[ActorRef[OutgoingAction]]] = byUuid.values.toMap

    val isComplete: Boolean = byPos.size >= PlayerPosition.All.size

    def allReceivers: Set[ActorRef[OutgoingAction]] = spectators ++ byPos.values.flatten

    def withPlayer(id: UUID, actorRef: ActorRef[OutgoingAction], pos: PlayerPosition): Clients = {
      val existingOpt = byUuid.get(id)
      if (existingOpt.exists { case (p, _) => p != pos }) {
        this
      } else {
        val existingRefs =
          existingOpt.fold(Set.empty[ActorRef[OutgoingAction]]) { case (_, refs) => refs }
        copy(byUuid + (id -> (pos, existingRefs + actorRef)))
      }
    }

    def withoutReceiver(id: UUID, actorRef: ActorRef[OutgoingAction]): Clients = {
      byUuid.get(id).fold(this) { case (pos, refs) =>
        copy(byUuid + (id -> (pos, refs - actorRef)))
      }
    }
  }

  private case class State(
    clients: Clients,
    tableState: FullTableState
  ) {

    private def tellAll(msg: OutgoingAction): Unit = {
      clients.allReceivers.foreach(_ ! msg)
    }

    def tellFullStateTo(
      actorRefs: Set[ActorRef[OutgoingAction]],
      posOpt: Option[PlayerPosition]
    ): Unit = {
      def tell[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
        val msg = NewMessageToClient(msgFactory(getA(tableState)))
        actorRefs.foreach(_ ! msg)
      }

      tell(_.playerNames, PlayersMessage.apply)
      tell(_.totalScores, TotalScoresMessage.apply)
      tell(_.missingPlayers, PlayersOnPauseMessage.apply)

      val gameState =
        posOpt.flatMap(tableState.playerStates.get).getOrElse(tableState.gameState.spectatorState)
      actorRefs.foreach(_ ! NewMessageToClient(GameStateMessage(gameState)))
    }

    def tellJoiningMessages(actorRef: ActorRef[OutgoingAction]): Unit = {
      actorRef ! NewMessageToClient(MessageToClient.Joining)
      actorRef ! NewMessageToClient(PlayersMessage(tableState.playerNames))
    }

    def tellWelcomeMessages(
      actorRef: ActorRef[OutgoingAction],
      posOpt: Option[PlayerPosition]
    ): Unit = {
      if (clients.isComplete) {
        tellFullStateTo(Set(actorRef), posOpt)
      } else {
        tellJoiningMessages(actorRef)
      }
    }

    def updateGameStateAndTellPlayers(
      newTableState: FullTableState,
      log: Logger,
      force: Boolean = false
    ): State = {

      def tellAllIfChanged[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
        val newA = getA(newTableState)
        if (getA(tableState) != newA || force) {
          val msg = NewMessageToClient(msgFactory(newA))
          tellAll(msg)
        }
      }

      tellAllIfChanged(_.playerNames, PlayersMessage.apply)
      tellAllIfChanged(_.totalScores, TotalScoresMessage.apply)
      tellAllIfChanged(_.missingPlayers, PlayersOnPauseMessage.apply)

      for {
        (pos, ps) <- newTableState.playerStates
        if force || !tableState.playerStates.get(pos).contains(ps)
        actors <- clients.byPos.get(pos)
        actor <- actors
      } {
        log.trace(s"Telling this to $pos: $ps")
        actor ! NewMessageToClient(GameStateMessage(ps))
      }
      if (
        clients.spectators.nonEmpty &&
        (force || tableState.gameState.spectatorState != newTableState.gameState.spectatorState)
      ) {
        val spectatorState = newTableState.gameState.spectatorState
        clients.spectators.foreach(_ ! NewMessageToClient(GameStateMessage(spectatorState)))
      }

      copy(tableState = newTableState)
    }

    def withPlayer(
      ctx: ActorContext[IncomingAction],
      id: UUID,
      actorRef: ActorRef[OutgoingAction],
      pos: PlayerPosition
    ): State = {
      ctx.watchWith(actorRef, PlayerReceiverDied(id, pos, actorRef))
      copy(clients = clients.withPlayer(id, actorRef, pos))
    }

    def withoutReceiver(id: UUID, actorRef: ActorRef[OutgoingAction]): State = {
      copy(clients = clients.withoutReceiver(id, actorRef))
    }

    def withSpectator(
      ctx: ActorContext[IncomingAction],
      actorRef: ActorRef[OutgoingAction]
    ): State = {
      ctx.watchWith(actorRef, SpectatorReceiverDied(actorRef))
      copy(clients = clients.copy(spectators = clients.spectators + actorRef))
    }

    def withoutSpectator(actorRef: ActorRef[OutgoingAction]): State = {
      copy(clients = clients.copy(spectators = clients.spectators - actorRef))
    }
  }

  def behavior(implicit rules: Rules): Behavior[IncomingAction] = behavior(State(rules = rules))

  // TODO: the "joining" logic probably should be moved into FullTableState (it does not depend on akka)

  private def behavior(state: State): Behavior[IncomingAction] =
    Behaviors.receive[IncomingAction] { (ctx, msg) =>
      ctx.log.trace(s"Received: $msg")
      msg match {
        case j: PlayerJoined if state.clients.byUuid.contains(j.playerId) =>
          val (pos, _) = state.clients.byUuid(j.playerId)
          val newGameState = state.tableState.playerRejoins(pos)
          val newState = state
            .withPlayer(ctx, j.playerId, j.replyTo, pos)
            .updateGameStateAndTellPlayers(newGameState, ctx.log)
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
            val newState = state.withPlayer(ctx, j.playerId, j.replyTo, pos)
            if (newState.clients.isComplete) {
              // reveal the initial game state:
              newState.updateGameStateAndTellPlayers(newState.tableState, ctx.log, force = true)
            } else {
              newState.tellJoiningMessages(j.replyTo)
            }
            behavior(newState)
          }
        case j: PlayerJoined =>
          ctx.log.info(s"Player tried to join when table as complete - joins as spectator")
          state.tellWelcomeMessages(j.replyTo, posOpt = None)
          behavior(state.withSpectator(ctx, j.replyTo))
        case d: PlayerReceiverDied if state.clients.byUuid.contains(d.playerId) =>
          ctx.log.info(s"Player ${d.playerId} left (${d.pos}, ${d.receiver.path.name})")
          val stateWithoutReceiver = state.withoutReceiver(d.playerId, d.receiver)
          val (pos, _) = stateWithoutReceiver.clients.byUuid(d.playerId)
          if (stateWithoutReceiver.clients.byPos(pos).nonEmpty) {
            behavior(stateWithoutReceiver)
          } else {
            val newState = stateWithoutReceiver.tableState.playerPauses(pos)
            behavior(stateWithoutReceiver.updateGameStateAndTellPlayers(newState, ctx.log))
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
              behavior(state.updateGameStateAndTellPlayers(newTableState, ctx.log))
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
            behavior(state.updateGameStateAndTellPlayers(gs, ctx.log))
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

  private object State {
    def apply(implicit rules: Rules): State = State(Clients(), FullTableState.apply)
  }

}

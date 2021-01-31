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
import io.github.mahh.doko.server.tableactor.IncomingAction.ReceiverDied
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
    byUuid: Map[UUID, (PlayerPosition, Set[ActorRef[OutgoingAction]])] = Map.empty
  ) {
    val byPos: Map[PlayerPosition, Set[ActorRef[OutgoingAction]]]= byUuid.values.toMap

    val isComplete: Boolean = byPos.size >= PlayerPosition.All.size

    def withPlayer(id: UUID, actorRef: ActorRef[OutgoingAction], pos: PlayerPosition): Players = {
      val existingOpt = byUuid.get(id)
      if (existingOpt.exists { case (p, _) => p != pos }) {
        this
      } else {
        val existingRefs =
          existingOpt.fold(Set.empty[ActorRef[OutgoingAction]]) { case (_, refs) => refs }
        copy(byUuid + (id -> (pos, existingRefs + actorRef)))
      }
    }

    def withoutReceiver(id: UUID, actorRef: ActorRef[OutgoingAction]): Players = {
      byUuid.get(id).fold(this) { case (pos, refs) =>
        copy(byUuid + (id -> (pos, refs - actorRef)))
      }
    }
  }

  private case class State(
    players: Players,
    tableState: FullTableState
  ) {

    def tellAll(msg: OutgoingAction): Unit = players.byPos.values.flatten.foreach(_ ! msg)


    def tellFullStateTo(player: UUID): Unit = {
      players.byUuid.get(player).foreach { case (pos, replyTos) =>
        def tell[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
          val msg = OutgoingAction.NewMessageToClient(msgFactory(getA(tableState)))
          replyTos.foreach(_ ! msg)
        }
        tell(_.playerNames, PlayersMessage.apply)
        tell(_.totalScores, TotalScoresMessage.apply)
        tell(_.missingPlayers, PlayersOnPauseMessage.apply)

        tableState.playerStates.get(pos).foreach { ps =>
          replyTos.foreach(_ ! OutgoingAction.NewMessageToClient(GameStateMessage(ps)))
        }
      }
    }

    def tellJoiningMessagesTo(actorRef: ActorRef[OutgoingAction]): Unit = {
      actorRef ! OutgoingAction.NewMessageToClient(MessageToClient.Joining)
      actorRef ! OutgoingAction.NewMessageToClient(PlayersMessage(tableState.playerNames))
    }

    def updateGameStateAndTellPlayers(
      newTableState: FullTableState,
      log: Logger,
      force: Boolean = false
    ): State = {

      def tellAllIfChanged[A](getA: FullTableState => A, msgFactory: A => MessageToClient): Unit = {
        val newA = getA(newTableState)
        if (getA(tableState) != newA || force) {
          val msg = OutgoingAction.NewMessageToClient(msgFactory(newA))
          tellAll(msg)
        }
      }

      tellAllIfChanged(_.playerNames, PlayersMessage.apply)
      tellAllIfChanged(_.totalScores, TotalScoresMessage.apply)
      tellAllIfChanged(_.missingPlayers, PlayersOnPauseMessage.apply)

      for {
        (pos, ps) <- newTableState.playerStates
        if force || !tableState.playerStates.get(pos).contains(ps)
        actors <- players.byPos.get(pos)
        actor <- actors
      } {
        log.trace(s"Telling this to $pos: $ps")
        actor ! OutgoingAction.NewMessageToClient(GameStateMessage(ps))
      }
      copy(tableState = newTableState)
    }

    def withPlayer(
      ctx: ActorContext[IncomingAction],
      id: UUID,
      actorRef: ActorRef[OutgoingAction],
      pos: PlayerPosition
    ): State = {
      ctx.watchWith(actorRef, ReceiverDied(id, pos, actorRef))
      copy(players = players.withPlayer(id, actorRef, pos))
    }

    def withoutReceiver(id: UUID, actorRef: ActorRef[OutgoingAction]): State = {
      copy(players = players.withoutReceiver(id, actorRef))
    }
  }

  def behavior(implicit rules: Rules): Behavior[IncomingAction] = behavior(State(rules = rules))

  // TODO: the "joining" logic probably should be moved into FullTableState (it does not depend on akka)

  private def behavior(state: State): Behavior[IncomingAction] = Behaviors.receive[IncomingAction] { (ctx, msg) =>
    ctx.log.trace(s"Received: $msg")
    msg match {
      case j: PlayerJoined if state.players.byUuid.contains(j.playerId) =>
        val (pos, _) = state.players.byUuid(j.playerId)
        val newGameState = state.tableState.playerRejoins(pos)
        val newState = state.withPlayer(ctx, j.playerId, j.replyTo, pos)
          .updateGameStateAndTellPlayers(newGameState, ctx.log)
        // make sure the player has the latest state (even if the browser was closed)
        if (!newState.players.isComplete) {
          newState.tellJoiningMessagesTo(j.replyTo)
        } else {
          newState.tellFullStateTo(j.playerId)
        }
        behavior(newState)
      case j: PlayerJoined if !state.players.isComplete =>
        val newPos: Option[PlayerPosition] =
          PlayerPosition.All.find(p => !state.players.byPos.contains(p))
        newPos.fold {
          // impossible to reach, but nevertheless:
          ctx.log.warn(s"Could not join even though not all positions are taken: $state")
          Behaviors.same[IncomingAction]
        } { pos =>
          val newState = state.withPlayer(ctx, j.playerId, j.replyTo, pos)
          if (newState.players.isComplete) {
            // reveal the initial game state:
            newState.updateGameStateAndTellPlayers(newState.tableState, ctx.log, force = true)
          } else {
            newState.tellJoiningMessagesTo(j.replyTo)
          }
          behavior(newState)
        }
      case j: PlayerJoined =>
        ctx.log.warn(s"Player tried to join when table as complete: $state")
        j.replyTo ! OutgoingAction.NewMessageToClient(MessageToClient.TableIsFull)
        j.replyTo ! OutgoingAction.Completed
        Behaviors.same
      case d: ReceiverDied if state.players.byUuid.contains(d.playerId) =>
        ctx.log.warn(s"Player ${d.playerId} left (${d.pos}, ${d.receiver.path.name})")
        val stateWithoutReceiver = state.withoutReceiver(d.playerId, d.receiver)
        val (pos, _) = stateWithoutReceiver.players.byUuid(d.playerId)
        if (stateWithoutReceiver.players.byPos(pos).nonEmpty) {
          behavior(stateWithoutReceiver)
        } else {
          val newState = stateWithoutReceiver.tableState.playerPauses(pos)
          behavior(stateWithoutReceiver.updateGameStateAndTellPlayers(newState, ctx.log))
        }
      case IncomingMessageFromClient(id, SetUserName(name)) =>
        state.players.byUuid.get(id).fold {
          ctx.log.debug(s"Unknown user id, cannot rename: $id")
          Behaviors.same[IncomingAction]
        } { case (pos, _) =>
          val newTableState = state.tableState.withUpdatedUserName(pos, name)
          behavior(state.updateGameStateAndTellPlayers(newTableState, ctx.log))
        }
      case IncomingMessageFromClient(id, PlayerActionMessage(action)) if state.players.isComplete =>
        val newGameState = for {
          (pos, _) <- state.players.byUuid.get(id)
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
      case l: PlayerLeaving =>
        // termination of incoming socket-stream is ignored - behavior reacts on corresponding
        // termination of outgoing socket-stream (on ReceiverDied)
        ctx.log.debug(s"Player ${l.playerId} leaving")
        Behaviors.same
      case d: ReceiverDied =>
        ctx.log.debug(s"A player left that was not even playing: $d")
        Behaviors.same
    }
  }

  private object State {
    def apply(implicit rules: Rules): State = State(Players(), FullTableState.apply)
  }

}

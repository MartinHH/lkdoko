package io.github.mahh.doko.server.tableactor

import java.util.UUID
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.Client
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
   * Holds `Ref`s of all connected clients (of one table).
   *
   * @param byUuid     The players by id.
   * @param spectators Spectators (not playing, but may see what is being played).
   * @tparam Ref The type of a reference to a client (e.g. an ActorRef).
   */
  private case class TableClients[Ref](
    byUuid: Map[UUID, (PlayerPosition, Set[Ref])] = Map.empty[UUID, (PlayerPosition, Set[Ref])],
    spectators: Set[Ref] = Set.empty[Ref]
  ) {
    val byPos: Map[PlayerPosition, Set[Ref]] = byUuid.values.toMap

    val isComplete: Boolean = byPos.size >= PlayerPosition.All.size

    def allReceivers: Set[Ref] = spectators ++ byPos.values.flatten

    def withPlayer(id: UUID, clientRef: Ref, pos: PlayerPosition): TableClients[Ref] = {
      val existingOpt = byUuid.get(id)
      if (existingOpt.exists { case (p, _) => p != pos }) {
        this
      } else {
        val existingRefs =
          existingOpt.fold(Set.empty[Ref]) { case (_, refs) => refs }
        copy(byUuid + (id -> (pos, existingRefs + clientRef)))
      }
    }

    def withoutReceiver(id: UUID, clientRef: Ref): TableClients[Ref] = {
      byUuid.get(id).fold(this) { case (pos, refs) =>
        copy(byUuid + (id -> (pos, refs - clientRef)))
      }
    }
  }

  private case class State[Ref](
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
      log: Logger,
      force: Boolean = false
    ): State[Ref] = {

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
        log.trace(s"Telling this to $pos: $ps")
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
    ): State[Ref] = {
      copy(clients = clients.withPlayer(id, actorRef, pos))
    }

    def withoutReceiver(id: UUID, clientRef: Ref): State[Ref] = {
      copy(clients = clients.withoutReceiver(id, clientRef))
    }

    def withSpectator(
      clientRef: Ref
    ): State[Ref] = {
      copy(clients = clients.copy(spectators = clients.spectators + clientRef))
    }

    def withoutSpectator(clientRef: Ref): State[Ref] = {
      copy(clients = clients.copy(spectators = clients.spectators - clientRef))
    }
  }

  given Client[ActorRef[OutgoingAction]] with
    extension (a: ActorRef[OutgoingAction])
      def tell(message: MessageToClient): Unit = a ! NewMessageToClient(message)

  def behavior(using rules: Rules): Behavior[IncomingAction] =
    behavior(State.apply)

  // TODO: the "joining" logic probably should be moved into FullTableState (it does not depend on akka)

  private def behavior(state: State[ActorRef[OutgoingAction]]): Behavior[IncomingAction] =
    Behaviors.receive[IncomingAction] { (ctx, msg) =>
      def withPlayer(j: PlayerJoined, pos: PlayerPosition): State[ActorRef[OutgoingAction]] = {
        ctx.watchWith(j.replyTo, PlayerReceiverDied(j.playerId, pos, j.replyTo))
        state.withPlayer(j.playerId, j.replyTo, pos)
      }
      ctx.log.trace(s"Received: $msg")
      msg match {
        case j: PlayerJoined if state.clients.byUuid.contains(j.playerId) =>
          val (pos, _) = state.clients.byUuid(j.playerId)
          val newGameState = state.tableState.playerRejoins(pos)
          val newState =
            withPlayer(j, pos).updateGameStateAndTellPlayers(newGameState, ctx.log)
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
            val newState = withPlayer(j, pos)
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
    def apply[Ref](using rules: Rules, c: Client[Ref]): State[Ref] =
      State(TableClients[Ref](), FullTableState.apply)
  }

}

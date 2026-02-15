package io.github.mahh.doko.server.tableactor

import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.ClientMessageTask
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.IncomingAction.ClientJoined
import io.github.mahh.doko.logic.table.IncomingAction.ClientLeft
import io.github.mahh.doko.logic.table.TableServerState
import io.github.mahh.doko.logic.table.TableServerStateMachine
import io.github.mahh.doko.server.tableactor.IncomingTableActorMessage.ClientLeaving
import io.github.mahh.doko.server.tableactor.IncomingTableActorMessage.WrappedIncomingAction
import io.github.mahh.doko.server.tableactor.OutgoingAction.NewMessageToClient
import io.github.mahh.doko.server.utils.logging.apply

/**
 * Actor holding (and updating) the state of one table.
 */
private[tableactor] object TableActor {

  private type ClientRef = ActorRef[OutgoingAction]

  def behavior(using rules: Rules): Behavior[IncomingTableActorMessage] =
    val initialState: TableServerState[ClientRef] = TableServerState.apply
    behavior(initialState)

  private def behavior(
    state: TableServerState[ClientRef]
  ): Behavior[IncomingTableActorMessage] =
    Behaviors.receive[IncomingTableActorMessage] { (ctx, msg) =>
      ctx.log.trace(s"Received: $msg")

      def handleIncomingAction(
        action: IncomingAction[ActorRef[OutgoingAction]]
      ): Behavior[IncomingTableActorMessage] =
        val result = TableServerStateMachine.transition(state, action)
        result.logTasks.foreach(ctx.log.apply)
        result.outgoingMessages.foreach { case ClientMessageTask(ref, msg) =>
          ref ! NewMessageToClient(msg)
        }
        behavior(result.state)

      msg match {
        case WrappedIncomingAction(action @ ClientJoined(clientId, id)) =>
          ctx.watchWith(clientId, WrappedIncomingAction(ClientLeft(clientId, id)))
          handleIncomingAction(action)
        case WrappedIncomingAction(action) =>
          handleIncomingAction(action)
        case l: ClientLeaving =>
          // termination of incoming socket-stream is ignored - behavior reacts on corresponding
          // termination of outgoing socket-stream (on ReceiverDied)
          ctx.log.debug(s"Player ${l.playerId} leaving")
          Behaviors.same
      }
    }

}

package io.github.mahh.doko.server.tableactor

import akka.NotUsed
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Flow
import akka.stream.typed.scaladsl.ActorSource
import io.github.mahh.doko.logic.game.FullTableState
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.ClientMessageTask
import io.github.mahh.doko.logic.table.TableClients
import io.github.mahh.doko.logic.table.TableServerState
import io.github.mahh.doko.logic.table.TableServerState.TableServerError
import io.github.mahh.doko.logic.table.TableServerState.TableServerError.PlayerActionError
import io.github.mahh.doko.logic.table.TableServerState.TransitionOutput
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.server.IncomingAction
import io.github.mahh.doko.server.IncomingAction.ClientLeft
import io.github.mahh.doko.server.IncomingAction.ClientJoined
import io.github.mahh.doko.server.IncomingAction.IncomingMessage
import io.github.mahh.doko.server.LogicFlowFactory
import io.github.mahh.doko.server.tableactor.IncomingTableActorMessage.ClientLeaving
import io.github.mahh.doko.server.tableactor.IncomingTableActorMessage.WrappedIncomingAction
import io.github.mahh.doko.server.tableactor.OutgoingAction.NewMessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerPosition
import org.slf4j.Logger

/**
 * Actor holding (and updating) the state of one table.
 */
private[tableactor] object TableActor {

  private type ClientRef = ActorRef[OutgoingAction]

  def behavior(using rules: Rules): Behavior[IncomingTableActorMessage] =
    val initialState: TableServerState[ClientRef] = TableServerState.apply
    behavior(initialState)

  /**
   * Sends out the outgoing messages and transitions to the next state.
   *
   * (Also allows to do some logging.)
   */
  private def performSuccessTransition(
    transitionOutput: TransitionOutput[ClientRef]
  )(
    log: Logger
  )(
    successLogging: Logger => Unit = _ => ()
  ): Behavior[IncomingTableActorMessage] = {
    val (newState, msgTasks) = transitionOutput
    successLogging(log)
    msgTasks.foreach { case ClientMessageTask(ref, msg) =>
      ref ! NewMessageToClient(msg)
    }
    behavior(newState)
  }

  private def performTransition[E](
    transitionOutput: Either[E, TransitionOutput[ClientRef]]
  )(
    log: Logger
  )(
    successLogging: Logger => Unit = _ => ()
  )(
    errorMsg: E => String
  ): Behavior[IncomingTableActorMessage] = {
    transitionOutput match
      case Right(output) =>
        performSuccessTransition(output)(log)(successLogging)
      case Left(e) =>
        log.debug(errorMsg(e))
        Behaviors.same
  }

  private def behavior(
    state: TableServerState[ClientRef]
  ): Behavior[IncomingTableActorMessage] =
    Behaviors.receive[IncomingTableActorMessage] { (ctx, msg) =>
      ctx.log.trace(s"Received: $msg")
      msg match {
        case WrappedIncomingAction(ClientJoined(clientId, id)) =>
          ctx.watchWith(clientId, WrappedIncomingAction(ClientLeft(clientId, id)))
          performSuccessTransition(state.applyClientJoined(id, clientId))(ctx.log) {
            _.info(s"Client $id joined")
          }
        case WrappedIncomingAction(cl @ ClientLeft(clientId, id)) =>
          performTransition(state.applyClientLeft(id, clientId))(ctx.log) {
            _.info(
              s"Client $id left (position=${state.clients.posForParticipant(id)})"
            )
          } { case TableServerError.UnknownClient =>
            s"A client left that was neither playing nor spectator: $cl"
          }
        case WrappedIncomingAction(IncomingMessage(id, SetUserName(name))) =>
          performTransition(state.applyUserNameChange(id, name))(ctx.log)() {
            case TableServerError.NonExistingPlayer =>
              s"Unknown user id, cannot rename: $id"
          }
        case WrappedIncomingAction(IncomingMessage(id, PlayerActionMessage(action))) =>
          performTransition(state.applyPlayerAction(id, action))(ctx.log)() {
            case TableServerError.ActionNotApplicable =>
              s"Action not applicable to state: $action -> $state"
            case TableServerError.PlayersIncomplete =>
              s"A player tried to trigger an action before table was completed ($id, $action)"
            case TableServerError.NonExistingPlayer =>
              s"Received a PlayerAction for an unknown player ($id, $action)"
          }
        case l: ClientLeaving =>
          // termination of incoming socket-stream is ignored - behavior reacts on corresponding
          // termination of outgoing socket-stream (on ReceiverDied)
          ctx.log.debug(s"Player ${l.playerId} leaving")
          Behaviors.same
      }
    }

}

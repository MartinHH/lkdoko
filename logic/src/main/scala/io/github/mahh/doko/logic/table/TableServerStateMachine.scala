package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.TableServerError.*
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.utils.logging.LogLevel.*
import io.github.mahh.doko.shared.utils.logging.LogTask

import scala.collection.immutable.Vector

/**
 * State machine logic for a "table".
 *
 * This describes an FSM where `TableServerState[ClientRef]` is the set of possible states,
 * `IncomingAction[ClientRef]` is the input alphabet and `Seq[ClientMessageTask[ClientRef]]`
 * is the output alphabet (along with some `LogTasks` for logging).
 */
object TableServerStateMachine {

  case class TransitionResult[ClientRef](
    state: TableServerState[ClientRef],
    outgoingMessages: Seq[ClientMessageTask[ClientRef]],
    logTasks: Seq[LogTask]
  )

  object TransitionResult {
    def initial[ClientRef](using rules: Rules): TransitionResult[ClientRef] =
      TransitionResult(
        TableServerState.apply[ClientRef],
        Vector.empty,
        Vector.empty
      )
  }

  private type TransitionEither[E <: TableServerError, ClientRef] =
    Either[E, TableServerState.StateAndOutput[ClientRef]]

  def transition[ClientRef](
    state: TableServerState[ClientRef],
    in: IncomingAction[ClientRef]
  ): TransitionResult[ClientRef] =
    def transition[E <: TableServerError](
      result: TransitionEither[E, ClientRef]
    )(
      successLogTasks: LogTask*
    )(
      errorLogging: E => LogTask
    ): TransitionResult[ClientRef] =
      result.fold(
        { e => TransitionResult(state, Vector.empty, Seq(errorLogging(e))) },
        { case (state, out) => TransitionResult(state, out, successLogTasks) }
      )
    import IncomingAction.*
    in match
      case ClientJoined(clientId, participantId) =>
        val (newState, out) = state.applyClientJoined(participantId, clientId)
        TransitionResult(
          newState,
          out,
          Seq(Info(s"Client for $participantId joined"))
        )
      case IncomingMessage(participantId, SetUserName(name)) =>
        transition[NonExistingPlayer.type](state.applyUserNameChange(participantId, name))() {
          case NonExistingPlayer =>
            Error(s"Unknown participantId, cannot rename: $participantId")
        }
      case IncomingMessage(id, PlayerActionMessage(action)) =>
        transition(state.applyPlayerAction(id, action))() {
          case ActionNotApplicable =>
            Warn(s"Action not applicable to state: $action -> $state")
          case PlayersIncomplete =>
            Warn(s"A player tried to trigger an action before table was completed ($id, $action)")
          case NonExistingPlayer =>
            Error(s"Received a PlayerAction for an unknown player ($id, $action)")
        }
      case cl @ ClientLeft(clientId, participantId) =>
        def pos: Option[PlayerPosition] = state.clients.posForParticipant(participantId)
        transition(state.applyClientLeft(participantId, clientId))(
          Info(s"Client for $participantId left (position=$pos)")
        ) { case TableServerError.UnknownClient =>
          Warn(s"A client left that was neither playing nor spectator: $cl")
        }

}

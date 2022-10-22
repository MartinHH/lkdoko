package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.msg.MessageToServer

/**
 * Input alphabet of the `TableServerStateMachine`-FSM.
 *
 * @tparam ClientRef The type of reference to individual clients. (This could be an `ActorRef`
 *                   or a `ClientId`.)
 */
trait IncomingAction[ClientRef]

object IncomingAction:
  case class ClientJoined[ClientRef](
    clientId: ClientRef,
    participantId: ParticipantId
  ) extends IncomingAction[ClientRef]

  case class IncomingMessage[ClientRef](
    participantId: ParticipantId,
    msg: MessageToServer
  ) extends IncomingAction[ClientRef]

  case class ClientLeft[ClientRef](
    clientId: ClientRef,
    participantId: ParticipantId
  ) extends IncomingAction[ClientRef]

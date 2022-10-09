package io.github.mahh.doko.server.tableactor

import akka.actor.typed.ActorRef
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.player.PlayerPosition

/**
 * Messages that are sent to the `GameActor`.
 */
private[tableactor] trait IncomingAction

private[tableactor] object IncomingAction {

  /**
   * Incoming socket connection has been opened.
   */
  case class ClientJoined(
    playerId: ParticipantId,
    replyTo: ActorRef[OutgoingAction]
  ) extends IncomingAction

  /**
   * Incoming message on opened socket connection.
   */
  case class IncomingMessageFromClient(
    playerId: ParticipantId,
    msg: MessageToServer
  ) extends IncomingAction

  /**
   * Incoming socket connection has been closed.
   */
  case class ClientLeaving(
    playerId: ParticipantId,
    failureOpt: Option[Throwable]
  ) extends IncomingAction

  /**
   * Incoming socket connection of a client has been fully disposed.
   *
   * (This is send via deathwatch when the actor for outgoing messages has died.)
   */
  case class ClientDied(
    clientId: ParticipantId,
    receiver: ActorRef[OutgoingAction]
  ) extends IncomingAction
}

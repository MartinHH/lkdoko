package io.github.mahh.doko.server.tableactor

import java.util.UUID

import akka.actor.typed.ActorRef
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.player.PlayerPosition

/**
 * Messages that are sent to the `GameActor`.
 */
private[server] trait IncomingAction

object IncomingAction {

  /**
   * Incoming socket connection has been opened.
   */
  case class ClientJoined(
    playerId: UUID,
    replyTo: ActorRef[OutgoingAction]
  ) extends IncomingAction

  /**
   * Incoming message on opened socket connection.
   */
  case class IncomingMessageFromClient(
    playerId: UUID,
    msg: MessageToServer
  ) extends IncomingAction

  /**
   * Incoming socket connection has been closed.
   */
  case class ClientLeaving(
    playerId: UUID,
    failureOpt: Option[Throwable]
  ) extends IncomingAction

  /**
   * Incoming socket connection of a client has been fully disposed.
   *
   * (This is send via deathwatch when the actor for outgoing messages has died.)
   */
  case class ClientDied(
    clientId: UUID,
    receiver: ActorRef[OutgoingAction]
  ) extends IncomingAction
}

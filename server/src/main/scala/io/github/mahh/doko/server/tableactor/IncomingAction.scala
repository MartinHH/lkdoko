package io.github.mahh.doko.server.tableactor

import java.util.UUID

import akka.actor.typed.ActorRef
import io.github.mahh.doko.shared.msg.MessageToServer

/**
 * Messages that are sent to the `GameActor`.
 */
private[server] trait IncomingAction {

  val playerId: UUID

}

object IncomingAction {

  case class PlayerJoined(
    playerId: UUID,
    replyTo: ActorRef[OutgoingAction]
  ) extends IncomingAction

  case class IncomingMessageFromClient(
    playerId: UUID,
    msg: MessageToServer
  ) extends IncomingAction

  case class PlayerLeft(
    playerId: UUID,
    failureOpt: Option[Throwable]
  ) extends IncomingAction
}

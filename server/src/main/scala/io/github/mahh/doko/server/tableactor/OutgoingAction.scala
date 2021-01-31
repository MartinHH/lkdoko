package io.github.mahh.doko.server.tableactor

import io.github.mahh.doko.shared.msg.MessageToClient

/**
 * Messages that are sent by the `GameActor`.
 */
trait OutgoingAction

object OutgoingAction {

  case class NewMessageToClient(msg: MessageToClient) extends OutgoingAction

  case object Completed extends OutgoingAction

}

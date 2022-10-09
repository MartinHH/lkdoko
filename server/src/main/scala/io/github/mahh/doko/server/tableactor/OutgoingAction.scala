package io.github.mahh.doko.server.tableactor

import io.github.mahh.doko.shared.msg.MessageToClient

/**
 * Messages that are sent by the `GameActor`.
 */
private[tableactor] trait OutgoingAction

private[tableactor] object OutgoingAction {

  case class NewMessageToClient(msg: MessageToClient) extends OutgoingAction

  case object Completed extends OutgoingAction

}

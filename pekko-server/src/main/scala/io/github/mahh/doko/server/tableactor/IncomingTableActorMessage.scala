package io.github.mahh.doko.server.tableactor

import org.apache.pekko.actor.typed.ActorRef
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.player.PlayerPosition

/**
 * Messages that are sent to the `TableActor`.
 *
 * Due to the nature of actors, this is a bit more complex than the regular
 * `IncomingAction`s, so this consists of a wrapper around `IncomingAction`
 * plus some additional message types.
 */
private[tableactor] trait IncomingTableActorMessage

private[tableactor] object IncomingTableActorMessage {

  case class WrappedIncomingAction(action: IncomingAction[ActorRef[OutgoingAction]])
    extends IncomingTableActorMessage

  /**
   * Incoming socket connection has been closed.
   */
  case class ClientLeaving(
    playerId: ParticipantId,
    failureOpt: Option[Throwable]
  ) extends IncomingTableActorMessage

}

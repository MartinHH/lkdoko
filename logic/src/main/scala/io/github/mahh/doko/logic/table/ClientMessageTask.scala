package io.github.mahh.doko.logic.table

import io.github.mahh.doko.shared.msg.MessageToClient

/**
 * A "named Tuple2" that can be used to describe the task of sending a message to a client.
 *
 * @tparam Ref The type of a reference to a client (e.g. an ActorRef).
 */
case class ClientMessageTask[Ref](clientRef: Ref, message: MessageToClient)

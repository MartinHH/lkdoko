package io.github.mahh.doko.logic.table

import io.github.mahh.doko.shared.msg.MessageToClient

/**
 * Typeclass abstracting over the ability to send a `MessageToClient` to a client.
 *
 * @tparam Ref The type of a reference to a client (e.g. an ActorRef).
 */
trait Client[Ref]:
  extension (a: Ref) def tell(message: MessageToClient): Unit

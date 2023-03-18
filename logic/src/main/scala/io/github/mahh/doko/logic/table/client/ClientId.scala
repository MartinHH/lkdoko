package io.github.mahh.doko.logic.table.client

/**
 * Default implementation of a reference to a client.
 */
opaque type ClientId = java.util.UUID

object ClientId:
  def random(): ClientId = java.util.UUID.randomUUID()

  private[client] def apply(uuid: java.util.UUID): ClientId = uuid

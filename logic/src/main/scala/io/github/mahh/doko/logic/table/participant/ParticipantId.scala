package io.github.mahh.doko.logic.table.participant

import java.util.UUID

/**
 * Id of a "participant" (either a player or a spectator).
 *
 * @note For each "participant", there could be multiple "clients" (e.g. browser windows).
 */
opaque type ParticipantId = UUID

object ParticipantId {

  def random(): ParticipantId = UUID.randomUUID()

  def fromString(name: String): ParticipantId = UUID.fromString(name)
}

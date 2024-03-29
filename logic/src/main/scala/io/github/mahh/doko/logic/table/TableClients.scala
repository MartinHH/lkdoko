package io.github.mahh.doko.logic.table

import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.player.PlayerPosition

import scala.collection.immutable.Map

/**
 * Holds `Ref`s of all connected clients (of one table).
 *
 * @param byParticipant The players by id.
 * @param spectators Spectators (not playing, but may see what is being played).
 * @tparam Ref The type of a reference to a client (e.g. an ActorRef).
 */
case class TableClients[Ref](
  players: Map[PlayerPosition, (ParticipantId, Set[Ref])],
  spectators: Set[Ref]
) {
  val byParticipant: Map[ParticipantId, (PlayerPosition, Set[Ref])] = players.map {
    case (pos, (pId, cIds)) => pId -> (pos, cIds)
  }

  val isComplete: Boolean = players.keySet == PlayerPosition.AllAsSet

  def allReceivers: Set[Ref] = spectators ++ players.values.flatMap { case (_, cIds) => cIds }

  def posForParticipant(id: ParticipantId): Option[PlayerPosition] =
    byParticipant.get(id).map { case (pos, _) => pos }

  def withPlayer(
    id: ParticipantId,
    clientRef: Ref,
    pos: PlayerPosition
  ): TableClients[Ref] = {
    val existingOpt = byParticipant.get(id)
    if (existingOpt.exists { case (p, _) => p != pos }) {
      this
    } else {
      val existingRefs =
        existingOpt.fold(Set.empty[Ref]) { case (_, refs) => refs }
      copy(players + (pos -> (id, existingRefs + clientRef)))
    }
  }

  def withoutReceiver(id: ParticipantId, clientRef: Ref): TableClients[Ref] = {
    byParticipant.get(id).fold(this) { case (pos, refs) =>
      copy(players + (pos -> (id, refs - clientRef)))
    }
  }
}

object TableClients {
  def empty[Ref]: TableClients[Ref] = TableClients(Map.empty, Set.empty)
}

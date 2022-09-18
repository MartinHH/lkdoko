package io.github.mahh.doko.logic.table

import io.github.mahh.doko.shared.player.PlayerPosition

import java.util.UUID
import scala.collection.immutable.Map

/**
 * Holds `Ref`s of all connected clients (of one table).
 *
 * @param byUuid     The players by id.
 * @param spectators Spectators (not playing, but may see what is being played).
 * @tparam Ref The type of a reference to a client (e.g. an ActorRef).
 */
case class TableClients[Ref](
  byUuid: Map[UUID, (PlayerPosition, Set[Ref])] = Map.empty[UUID, (PlayerPosition, Set[Ref])],
  spectators: Set[Ref] = Set.empty[Ref]
) {
  val byPos: Map[PlayerPosition, Set[Ref]] = byUuid.values.toMap

  val isComplete: Boolean = byPos.size >= PlayerPosition.All.size

  def allReceivers: Set[Ref] = spectators ++ byPos.values.flatten

  def withPlayer(id: UUID, clientRef: Ref, pos: PlayerPosition): TableClients[Ref] = {
    val existingOpt = byUuid.get(id)
    if (existingOpt.exists { case (p, _) => p != pos }) {
      this
    } else {
      val existingRefs =
        existingOpt.fold(Set.empty[Ref]) { case (_, refs) => refs }
      copy(byUuid + (id -> (pos, existingRefs + clientRef)))
    }
  }

  def withoutReceiver(id: UUID, clientRef: Ref): TableClients[Ref] = {
    byUuid.get(id).fold(this) { case (pos, refs) =>
      copy(byUuid + (id -> (pos, refs - clientRef)))
    }
  }
}

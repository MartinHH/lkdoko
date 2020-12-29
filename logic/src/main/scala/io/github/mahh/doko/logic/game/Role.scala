package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.game.Reservation

/**
 * Describes a role that a player can have in the game.
 */
private[logic] sealed trait Role

private[logic] object Role {

  case object Re extends Role

  case object Kontra extends Role

  /** Player has both queens of spades and has called the reservation "marriage". */
  case object Marriage extends Role

  /** Player has both queens of spades but did not call the reservation "marriage". */
  case object SilentMarriage extends Role

  /** Player was in `Marriage` role but did not find a partner. */
  case object MarriageSolo extends Role

  /** Player is the partner of a `Marriage` player. */
  case object Married extends Role

  case class Solo(soloType: Reservation.Solo) extends Role

  /**
   * Returns true if the role is considered as the "elders" (which is the dominant team).
   */
  def isElders(role: Role): Boolean = role != Kontra

}
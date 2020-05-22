package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.game.Reservation

sealed trait Role

object Role {

  case object Re extends Role

  case object Kontra extends Role

  case object Marriage extends Role

  case object SilentMarriage extends Role

  /** Non-silent marriage, but no partner was found. */
  case object MarriageSolo extends Role

  case object Married extends Role

  case class Solo(soloType: Reservation.Solo) extends Role

  case object Poverty extends Role

  val isElders: Role => Boolean = _ != Kontra

}
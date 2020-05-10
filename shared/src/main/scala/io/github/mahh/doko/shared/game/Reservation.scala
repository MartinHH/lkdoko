package io.github.mahh.doko.shared.game

import io.github.mahh.doko.shared.rules.Trumps

/**
 * Aka "Vorbehalt".
 */
sealed trait Reservation

object Reservation {

  sealed trait LastingReservation extends Reservation

  final case class Solo(solo: Trumps.Solo) extends LastingReservation

  object Solo {
    val All: List[Solo] = Trumps.Solo.All.map(Solo.apply)
  }

  case object Throwing extends Reservation

  case object Poverty extends Reservation

  case object Marriage extends LastingReservation

}

package io.github.mahh.doko.shared.game

import io.github.mahh.doko.shared.json.Json._
import io.github.mahh.doko.shared.rules.Trumps

/**
 * Aka "Vorbehalt".
 */
sealed trait Reservation derives Encoder, Decoder

object Reservation {

  sealed trait LastingReservation extends Reservation

  final case class Solo(solo: Trumps.Solo) extends LastingReservation

  object Solo {
    val All: List[Solo] = Trumps.Solo.All.map(Solo.apply)
    val AllAsSet: Set[Solo] = All.toSet
  }

  case object Throwing extends Reservation

  case object Poverty extends Reservation

  case object Marriage extends LastingReservation

}

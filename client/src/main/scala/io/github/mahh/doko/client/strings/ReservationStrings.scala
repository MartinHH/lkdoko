package io.github.mahh.doko.client.strings

import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Reservation._
import io.github.mahh.doko.shared.rules.Trumps

// TODO: replace language mechanism by whatever the js/scala.js way to this is

trait ReservationStrings {
  def toString(reservation: Option[Reservation]): String
}

object ReservationStrings {

  def default: ReservationStrings = German

  object German extends ReservationStrings {
    override def toString(reservation: Option[Reservation]): String = reservation.fold {
      "Kein Vorbehalt"
    } {
      case Solo(s) =>
        val prefix = s match {
          case Trumps.Solo.QueensSolo   => "Damen"
          case Trumps.Solo.JacksSolo    => "Buben"
          case Trumps.Solo.ClubsSolo    => "Kreuz"
          case Trumps.Solo.SpadesSolo   => "Pik"
          case Trumps.Solo.HeartsSolo   => "Herz"
          case Trumps.Solo.DiamondsSolo => "Karo"
        }
        prefix + "solo"
      case Throwing =>
        "Schmeissen"
      case Poverty =>
        "Armut"
      case Marriage =>
        "Hochzeit"
    }
  }

}

package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.deck._
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.game.Reservation.Marriage
import io.github.mahh.doko.shared.game.Reservation.Poverty
import io.github.mahh.doko.shared.game.Reservation.Solo
import io.github.mahh.doko.shared.game.Reservation.Throwing
import io.github.mahh.doko.shared.rules.Trumps

private[game] object Reservations {

  /**
   * Calculates the "reservations" a player could call holding the given `initialCards`.
   */
  def possibleReservations(initialCards: Seq[Card]): List[Reservation] = {

    def oneOrNone[C <: Reservation](c: C)(p: Boolean): List[C] =
      if (p) List(c) else List.empty

    val throwing: List[Throwing.type] = oneOrNone(Throwing) {
      val kings = initialCards.count(_.rank == Rank.K)
      val nines = initialCards.count(_.rank == Rank.Nine)
      val fulls = initialCards.count(_.value >= Rank.Ten.value)
      kings >= 5 || nines >= 5 || (kings == 4 && nines == 4) || fulls >= 7
    }

    val poverty: List[Poverty.type] = oneOrNone(Poverty) {
      val trumps = initialCards.count(Trumps.Default.isTrump)

      def hasFox: Boolean = initialCards.contains(Fox)

      trumps <= 3 || (trumps == 4 && hasFox)
    }

    val marriage: List[Marriage.type] = oneOrNone(Marriage) {
      initialCards.count(_ == QueenOfClubs) >= 2
    }

    Solo.All ::: throwing ::: poverty ::: marriage
  }
}

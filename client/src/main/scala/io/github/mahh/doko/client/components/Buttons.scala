package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.BidsConfig
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.game.Reservation

object Buttons {

  private def bidButton(
    config: Signal[BidsConfig],
    bid: Bid,
    handler: Bid => Unit
  ): Button =
    button(
      child.text <-- config.map(c => BidStrings.default.toString(NameableBid(c.isElders, bid))),
      disabled <-- config.map(c => c.possibleBid.forall(Bid.ordering.lt(bid, _))),
      onClick --> ((_: org.scalajs.dom.MouseEvent) => handler(bid)),
      cls := "bid-button"
    )

  def bidButtons(
    config: Signal[BidsConfig],
    handler: Bid => Unit
  ): Div =
    val all = Bid.All.map(bidButton(config, _, handler))
    div(
      all
    )

  private def noReservationButton(
    handler: () => Unit
  ): Button =
    val txt = ReservationStrings.default.toString(None)
    button(
      txt,
      onClick --> ((_: org.scalajs.dom.MouseEvent) => handler()),
      cls := "no-reservation-button"
    )
  private def reservationButton(
    possibleReservations: Signal[Option[Set[Reservation]]],
    reservation: Reservation,
    handler: Reservation => Unit,
    clss: String
  ): Button =
    val txt = ReservationStrings.default.toString(Some(reservation))
    button(
      txt,
      disabled <-- possibleReservations.map(_.forall(reservations => !reservations(reservation))),
      onClick --> ((_: org.scalajs.dom.MouseEvent) => handler(reservation)),
      cls := clss
    )

  def reservationButtons(
    possibleReservations: Signal[Option[Set[Reservation]]],
    handler: Option[Reservation] => Unit
  ): Div =
    val someHandler: Reservation => Unit = r => handler(Some(r))
    val noneHandler: () => Unit = () => handler(None)
    val noneButton = noReservationButton(noneHandler)
    val solos = Reservation.Solo.All.map(
      reservationButton(possibleReservations, _, someHandler, "solo-reservation-button")
    )
    val others = List(Reservation.Throwing, Reservation.Poverty, Reservation.Marriage).map(
      reservationButton(possibleReservations, _, someHandler, "special-reservation-button")
    )
    div(
      hidden <-- possibleReservations.map(_.isEmpty),
      div(
        noneButton,
        cls := "reservation-button-block"
      ),
      div(
        solos,
        cls := "reservation-button-block"
      ),
      div(
        others,
        cls := "reservation-button-block"
      )
    )
}

package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.AckConfig
import io.github.mahh.doko.client.state.BidsConfig
import io.github.mahh.doko.client.state.ConfigurableCountdown
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.Acknowledgement
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReply

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
    handler: PlayerAction.PlaceBid => Unit
  ): Div =
    val all = Bid.All.map(bidButton(config, _, bid => handler(PlayerAction.PlaceBid(bid))))
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
    handler: PlayerAction.CallReservation => Unit
  ): Div =
    val someHandler: Reservation => Unit = r => handler(PlayerAction.CallReservation(Some(r)))
    val noneHandler: () => Unit = () => handler(PlayerAction.CallReservation(None))
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

  /**
   * Button for acknowledging + controls the auto-acknowledging.
   */
  def countdownAckButton(
    ackConfig: Signal[Option[AckConfig]],
    actionSink: PlayerAction[GameState] => Unit
  ): Div =
    val checkInitially = true
    val active = Var(checkInitially)
    val activeTimeOut: Signal[Option[Int]] =
      active.toObservable.combineWithFn(ackConfig.map(_.flatMap(_.autoAckTimeout))) { (a, to) =>
        to.filter(_ => a)
      }
    val countdown = ConfigurableCountdown.countDown(activeTimeOut)
    val autoAcks: EventStream[PlayerAction[GameState]] = countdown
      .combineWithFn(ackConfig) { (cdOpt, confOpt) =>
        for {
          cd <- cdOpt
          if cd <= 0
          conf <- confOpt
        } yield conf.ack
      }
      .changes
      .collect { case Some(ack) => ack }
    val clickEventStream = new EventBus[org.scalajs.dom.MouseEvent]
    val clickActions: Observable[PlayerAction[GameState]] =
      ackConfig.changes
        .collect { case Some(c) => c.ack }
        .flatMap(ack => clickEventStream.toObservable.map(_ => ack))(SwitchStreamStrategy)
    div(
      button(
        child.text <-- countdown.map(_.fold("OK")(c => s"OK ($c)")),
        disabled <-- ackConfig.map(_.isEmpty),
        onClick --> clickEventStream,
        clickActions --> actionSink,
        autoAcks --> actionSink,
        cls := "bid-button"
      ),
      input(
        typ("checkbox"),
        onInput.mapToChecked --> active,
        checked(checkInitially)
      ),
      span("Auto-OK")
    )

}

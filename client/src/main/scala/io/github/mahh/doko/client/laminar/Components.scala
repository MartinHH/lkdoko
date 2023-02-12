package io.github.mahh.doko.client.laminar

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.CardConfig
import io.github.mahh.doko.client.state.BidsConfig
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

object Components {

  def nameInput(
    disabledSignal: SignalSource[Boolean],
    commandSink: SetUserName => Unit
  ): Div =
    div(
      label(
        "Your name: ",
        hidden <-- disabledSignal
      ),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        hidden <-- disabledSignal,
        onInput.mapToValue.map(SetUserName.apply) --> commandSink
      ),
      p()
    )

  def announcement(
    contentObservable: Observable[Option[String]]
  ): Div =
    div(
      child.maybe <-- contentObservable.map(_.map(txt => span(txt)))
    )

  private def execute(callback: () => Unit): Unit = callback()

  def card(
    config: Signal[CardConfig],
    clss: String
  ): Image =
    val clickEventStream = new EventBus[org.scalajs.dom.MouseEvent]
    val clickCallbacks: Observable[() => Unit] =
      config.flatMap(c => clickEventStream.toObservable.map(_ => c.callback))(SwitchStreamStrategy)
    img(
      src <-- config.map(_.imageSrc),
      onClick --> clickEventStream,
      clickCallbacks --> execute,
      cls := clss
    )

  def trick(
    trick: Signal[Map[PlayerPosition, CardConfig]]
  ): Div =
    val allFour: Signal[List[CardConfig]] =
      trick.map(tm => PlayerPosition.All.map(pos => tm.getOrElse(pos, CardConfig(None))))
    val placeHolder = card(Signal.fromValue(CardConfig(None)), "trick-card")
    val cardStream: Signal[List[Image]] =
      allFour
        .map(_.zipWithIndex)
        .split { case (_, i) => i } { (_, _, indexedCard) =>
          card(indexedCard.map { case (c, _) => c }, "trick-card")
        }
        .map(placeHolder +: _)
    div(
      children <-- cardStream
    )

  def hand(
    hand: Signal[Seq[CardConfig]]
  ): Div =
    val cards: Signal[Seq[Node]] =
      hand.map(_.zipWithIndex).split { case (_, i) => i } { (_, _, indexedCard) =>
        div(
          card(indexedCard.map { case (c, _) => c }, "hand-card"),
          cls := "hand-card-container"
        )
      }
    div(
      children <-- cards
    )

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
}

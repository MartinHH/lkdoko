package io.github.mahh.doko.client.laminar

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.SvgPaths
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName

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

  // there are at most 16 cards to display on the full window width => 100 / 16 = 6.25
  private val cardWidth = "6.2vw"

  def card(
    card: Card,
    clickHandler: Card => Unit
  ): Image =
    val uri = SvgPaths.getSvgUri(card)
    val handle: Any => Unit = _ => clickHandler(card)
    img(
      src(uri),
      textArea(uri),
      onClick --> handle,
      width := cardWidth
    )

  def cardPlaceholder: Div =
    div(
      width := cardWidth
    )

  def hand(
    cards: Seq[Card],
    clickHandler: Card => Unit
  ): Div =
    val childCards: Children = cards.map(card(_, clickHandler))
    div(
      children <-- Signal.fromValue(childCards)
    )
}

package io.github.mahh.doko.client

import io.github.mahh.doko.shared.deck.Card
import org.scalajs.dom
import org.scalajs.dom.raw.Element
import org.scalajs.dom.raw.HTMLDivElement
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.raw.HTMLInputElement

/**
 * Factories for various [[Element]]s.
 */
object ElementFactory {

  def createElement[E <: Element](tagName: String): E = {
    dom.document.createElement(tagName).asInstanceOf[E]
  }

  private def createDiv: HTMLDivElement = createElement("div")

  def p(msg: String): Element = {
    val paragraph: Element = createElement("p")
    paragraph.innerHTML = msg
    paragraph
  }

  def tableRowDiv: HTMLDivElement = {
    val div = createDiv
    div.style = "display: table-row;"
    div
  }

  def tableCellDiv(content: String): HTMLDivElement = {
    val div = createDiv
    div.style = "display: table-cell; padding: 0px 5px;"
    div.innerHTML = content
    div
  }

  def handElement(
    hand: Iterable[Card],
    handler: Card => Option[() => Unit] = _ => None,
    cardHeight: Int
  ): HTMLDivElement = {
    val cards = createDiv
    hand.foreach { card =>
      cards.appendChild(cardElement(card, handler(card), cardHeight))
    }
    cards
  }

  def cardElement(card: Card, handler: Option[() => Unit], cardHeight: Int): HTMLImageElement = {
    val c: HTMLImageElement = createElement("img")
    val uri = SvgPaths.getSvgUri(card)
    c.src = uri
    c.textContent = SvgPaths.getSvgUri(card)
    c.height = cardHeight
    handler.foreach { h =>
      c.onclick = _ => h()
    }
    c
  }

  def buttonElement(
    title: String,
    onClick: () => Unit,
    actionCountDownOpt: Option[ActionCountDown] = None
  ): HTMLInputElement = {
    val button: HTMLInputElement = createElement("input")
    button.`type` = "button"
    button.value = title

    actionCountDownOpt.foreach { countDown =>
      countDown.addCountDownCallback { remaining =>
        button.value = s"$title ($remaining)"
      }
    }

    button.onclick = _ => {
      onClick()
    }
    button
  }
}

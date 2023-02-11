package io.github.mahh.doko.client

import io.github.mahh.doko.shared.deck.Card
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLInputElement

/**
 * Factories for various [[Element]]s.
 */
object ElementFactory {

  def createElement[E <: Element](tagName: String): E = {
    dom.document.createElement(tagName).asInstanceOf[E]
  }

  def p(msg: String): Element = {
    val paragraph: Element = createElement("p")
    paragraph.innerHTML = msg
    paragraph
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
      countDown.addCountDownCallback { remainingOpt =>
        button.value = remainingOpt.fold(title)(remaining => s"$title ($remaining)")
      }
    }

    button.onclick = _ => {
      onClick()
    }
    button
  }
}

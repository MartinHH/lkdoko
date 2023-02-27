package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReply
import org.scalajs.dom.html

/**
 * Components that combine multiple UI elements to an "area".
 */
object Areas {

  /**
   * Multi-purpose ("announcement") text field with optional buttons.
   */
  // TODO: the "poverty return" button should be integrated here as well.
  def announcement(
    contentObservable: Observable[String],
    povertyOffered: Signal[Boolean],
    actionSink: PovertyReply => Unit
  ): Div =
    def b(title: String, action: Boolean) =
      button(
        title,
        onClick --> ((_) => actionSink(PovertyReply(action))),
        visibility <-- povertyOffered.map(v => if (v) "visible" else "hidden")
      )
    div(
      span(
        child.text <-- contentObservable
      ),
      b("Annehmen", true),
      b("Ablehnen", false),
      cls := "announcement-area"
    )
}

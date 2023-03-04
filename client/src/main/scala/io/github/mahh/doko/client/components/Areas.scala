package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.AnnouncementButtonsConfig
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReply
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReturn

/**
 * Components that combine multiple UI elements to an "area".
 */
object Areas {

  /**
   * Multi-purpose ("announcement") text field with optional buttons.
   */
  def announcement(
    contentObservable: Observable[String],
    buttonsConfig: Signal[AnnouncementButtonsConfig],
    actionSink: PlayerAction[GameState] => Unit
  ): Div =
    import AnnouncementButtonsConfig.*
    def b(
      title: AnnouncementButtonsConfig => String,
      action: AnnouncementButtonsConfig => Option[PlayerAction[GameState]],
      enabled: AnnouncementButtonsConfig => Boolean
    ): Button =
      val clickEventStream = new EventBus[org.scalajs.dom.MouseEvent]
      val actions = buttonsConfig.map(action)
      val clickActions: Observable[PlayerAction[GameState]] =
        actions
          .flatMap(aOpt => clickEventStream.toObservable.map(_ => aOpt))(SwitchStreamStrategy)
          .collect { case Some(action) => action }
      button(
        child.text <-- buttonsConfig.map(title),
        clickActions --> actionSink,
        onClick --> clickEventStream,
        visibility <-- actions.map(a => if (a.nonEmpty) "visible" else "hidden"),
        disabled <-- buttonsConfig.map(c => !enabled(c))
      )
    div(
      span(
        child.text <-- contentObservable
      ),
      b(
        {
          case NoButtons          => ""
          case PovertyOffered     => "Annehmen"
          case PovertyExchange(_) => "Fertig"
        },
        {
          case NoButtons          => Option.empty
          case PovertyOffered     => Some(PovertyReply(true))
          case PovertyExchange(_) => Some(PovertyReturn)
        },
        {
          case PovertyExchange(canReturn) => canReturn
          case _                          => true
        }
      ),
      b(
        {
          case PovertyOffered => "Ablehnen"
          case _              => ""
        },
        {
          case PovertyOffered => Some(PovertyReply(false))
          case _              => None
        },
        _ => true
      ),
      cls := "announcement-area"
    )
}

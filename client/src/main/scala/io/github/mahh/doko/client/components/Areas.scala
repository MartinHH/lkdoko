package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.AnnouncementButtonsConfig
import io.github.mahh.doko.client.state.ConnectionState
import io.github.mahh.doko.client.state.ConnectionState.Connected
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReply
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReturn
import io.laminext.syntax.core.*

/**
 * Components that combine multiple UI elements to an "area".
 */
object Areas {

  /**
   * Combines name input and visualization of connection state.
   */
  def nameAndState(
    isAllowed: Signal[Boolean],
    connectionState: Signal[ConnectionState],
    missingPlayers: Signal[Set[String]],
    commandSink: Observer[SetUserName]
  ): Div =
    val stateColor = connectionState.combineWithFn(missingPlayers) {
      case (Connected, m) if m.isEmpty             => "green"
      case (Connected, _)                          => "grey"
      case (ConnectionState.Disconnected(true), _) => "orange"
      case _                                       => "red"
    }
    div(
      label(
        "Your name: "
      ),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        onInput.mapToValue.map(SetUserName.apply) --> commandSink,
        disabled <-- isAllowed.not
      ),
      span(
        backgroundColor <-- stateColor,
        cls := "state-dot"
      ),
      span(
        color <-- stateColor,
        child.text <-- connectionState.combineWithFn(missingPlayers) {
          case (Connected, m) if m.nonEmpty    => s"Warten auf: ${m.mkString(", ")}"
          case (ConnectionState.Error(msg), _) => s"Fehler: $msg"
          case _                               => ""
        }
      )
    )

  /**
   * Multi-purpose ("announcement") text field with optional buttons.
   */
  def announcement(
    contentObservable: Observable[String],
    buttonsConfig: Signal[AnnouncementButtonsConfig],
    actionSink: Observer[PlayerAction[GameState]]
  ): Div =
    import AnnouncementButtonsConfig.*
    def b(
      title: AnnouncementButtonsConfig => String,
      action: AnnouncementButtonsConfig => Option[PlayerAction[GameState]],
      enabled: AnnouncementButtonsConfig => Boolean
    ): Button =
      val actions = buttonsConfig.map(action)
      button(
        child.text <-- buttonsConfig.map(title),
        observeClicksWithActions(actions, actionSink),
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

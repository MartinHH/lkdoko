package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement.Base
import io.github.mahh.doko.client.state.ClientState
import io.github.mahh.doko.client.state.ConnectionState
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.player.PlayerAction
import io.laminext.websocket.WebSocket
import io.laminext.websocket.WebSocketEvent

/**
 * "Root" component that combines all other components.
 */
object App {

  def apply(
    ws: WebSocket[MessageToClient, MessageToServer]
  ): Div =
    val state = Var(ClientState.initial)
    val wsObserver = state.updater[MessageToClient](_.update(_))
    def sig[A](f: ClientState => A): Signal[A] = state.toObservable.map(f)
    val actionSink: Observer[PlayerAction[GameState]] =
      ws.send.contramap(PlayerActionMessage.apply)
    div(
      ws.connect,
      ws.received --> wsObserver,
      Areas
        .nameAndState(sig(_.nameInputAllowed), ws.connectionState, sig(_.missingPlayers), ws.send),
      p(),
      Tables.gameTable(state.toObservable),
      Areas.announcement(sig(_.announcementString), sig(_.buttonsConfig), actionSink),
      Cards.trick(sig(_.trick), actionSink),
      p(),
      Buttons.bidButtons(sig(_.bidsConfig), actionSink),
      p(),
      Cards.hand(sig(_.hand), actionSink),
      Buttons.reservationButtons(sig(_.possibleReservations), actionSink),
      Tables.roundResultsTable(sig(_.results), sig(_.playerNames)),
      Buttons.countdownAckButton(sig(_.ackConfig), actionSink)
    )

  extension (ws: WebSocket[MessageToClient, MessageToServer])
    def connectionState: Signal[ConnectionState] =
      ws.events
        .collect {
          case _: WebSocketEvent.Connected => ConnectionState.Connected
          case c: WebSocketEvent.Closed    => ConnectionState.Disconnected(c.willReconnect)
          case WebSocketEvent.Error(error) => ConnectionState.Error(error.getMessage)
        }
        .toSignal(ConnectionState.Disconnected(true))
}

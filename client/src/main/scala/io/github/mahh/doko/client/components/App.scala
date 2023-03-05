package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.ClientState
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.player.PlayerAction
import io.laminext.websocket.WebSocket

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
      StringComponents.nameInput(sig(_.nameInputAllowed), ws.send),
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

}

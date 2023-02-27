package io.github.mahh.doko.client

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.EventStream
import com.raquo.laminar.api.L.Signal
import com.raquo.laminar.api.L.Var
import com.raquo.laminar.api.L.windowEvents
import com.raquo.laminar.nodes.ReactiveElement.Base
import io.github.mahh.doko.client.components.*
import io.github.mahh.doko.client.state.ClientGameState
import io.github.mahh.doko.client.state.Signals
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.json.Json
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
import io.github.mahh.doko.shared.msg.MessageToClient.Joining
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersMessage
import io.github.mahh.doko.shared.msg.MessageToClient.PlayersOnPauseMessage
import io.github.mahh.doko.shared.msg.MessageToClient.TableIsFull
import io.github.mahh.doko.shared.msg.MessageToClient.TotalScoresMessage
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.score.TotalScores
import org.scalajs.dom

/**
 * The client's main code.
 */
object Client {

  @inline private def renderOnDomContentLoaded(
    selectors: => String,
    rootNode: => Base
  ): Unit = L.renderOnDomContentLoaded(
    dom.document.querySelector(selectors),
    rootNode
  )

  private val signals = new Signals

  private val nameInputHidden = Var(true)

  def main(args: Array[String]): Unit = {

    val socket: Socket = new Socket

    def actionSink(action: PlayerAction[GameState]): Unit = {
      socket.write(PlayerActionMessage(action))
    }

    socket.setListener(new Socket.Listener {
      override def onOpen(isReconnect: Boolean): Unit = {
        if (!isReconnect) {
          nameInputHidden.set(false)
        }
      }

      override def onError(msg: String): Unit = {
        signals.updateClientGameState(ClientGameState.Error(s"Failed: $msg"))
      }

      override def onUpdate(update: Either[Json.DecodeError, MessageToClient]): Unit =
        update match {
          case Left(error) =>
            val msg = s"Error reading message from server: $error"
            signals.updateClientGameState(ClientGameState.Error(msg))
          case Right(Joining) =>
            signals.updateClientGameState(ClientGameState.Joining)
          case Right(GameStateMessage(gs)) =>
            signals.updateClientGameState(ClientGameState.GameInProgress(gs))
          case Right(PlayersMessage(players)) =>
            signals.updatePlayerNames(players)
          case Right(TotalScoresMessage(scores)) =>
            signals.updateTotalScores(scores)
          case Right(PlayersOnPauseMessage(_)) =>
          // one or more players are having connection troubles
          // TODO: notify user that she needs to wait until all players are back
          case Right(TableIsFull) =>
            nameInputHidden.set(true)
        }
    })

    renderOnDomContentLoaded("#namearea", StringComponents.nameInput(nameInputHidden, socket.write))
    renderOnDomContentLoaded(
      "#announcements",
      Areas.announcement(signals.announcementString, signals.povertyOffered, actionSink)
    )

    renderOnDomContentLoaded(
      "#gametable",
      Tables.gameTable(
        signals.playerNames,
        signals.playerMarker,
        signals.bids,
        signals.trickCounts,
        signals.totalScores
      )
    )

    renderOnDomContentLoaded(
      "#bidbuttons",
      Buttons.bidButtons(signals.bidsConfig, b => actionSink(PlayerAction.PlaceBid(b)))
    )
    renderOnDomContentLoaded("#trick", Cards.trick(signals.trick, actionSink))
    renderOnDomContentLoaded("#hand", Cards.hand(signals.hand, actionSink))
    renderOnDomContentLoaded(
      "#reservations",
      Buttons.reservationButtons(
        signals.possibleReservations,
        r => actionSink(PlayerAction.CallReservation(r))
      )
    )
    renderOnDomContentLoaded(
      "#results",
      Tables.roundResultsTable(signals.results, signals.playerNames)
    )
    renderOnDomContentLoaded("#ack", Buttons.countdownAckButton(signals.ackConfig, actionSink))
  }

}

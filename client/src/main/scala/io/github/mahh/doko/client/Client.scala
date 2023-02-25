package io.github.mahh.doko.client

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.EventStream
import com.raquo.laminar.api.L.Signal
import com.raquo.laminar.api.L.Var
import com.raquo.laminar.api.L.windowEvents
import com.raquo.laminar.nodes.ReactiveElement.Base
import io.github.mahh.doko.client.components.*
import io.github.mahh.doko.client.state.Signals
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.*
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.Accepting
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.NotInvolved
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.Poor
import io.github.mahh.doko.shared.game.Reservation
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
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
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

  private def playerName(pos: PlayerPosition): String =
    signals.playerName(pos)

  private val signals = new Signals

  private val nameInputHidden = Var(true)

  private val announcement = Var(Option.empty[String])

  private def announce(msg: String): Unit = announcement.set(Some(msg))
  private def clearAnnouncement(): Unit = announcement.set(None)

  def main(args: Array[String]): Unit = {

    announce("Joining...")

    val socket: Socket = new Socket

    def actionSink(action: PlayerAction[GameState]): Unit = {
      socket.write(PlayerActionMessage(action))
    }

    socket.setListener(new Socket.Listener {
      override def onOpen(isReconnect: Boolean): Unit = {
        if (!isReconnect) {
          announce("Connection was successful!")
          nameInputHidden.set(false)
        }
      }

      override def onError(msg: String): Unit = {
        announce(s"Failed: $msg")
      }

      override def onUpdate(update: Either[Json.DecodeError, MessageToClient]): Unit =
        update match {
          case Left(error) =>
            announce(s"Error reading message from server: $error")
          case Right(Joining) =>
            announce("Waiting for others to join...")
          case Right(GameStateMessage(gameState)) =>
            clearAnnouncement()
            GameStateHandlers.handleGameState(gameState, actionSink)
          case Right(PlayersMessage(players)) =>
            signals.updatePlayerNames(players)
          case Right(TotalScoresMessage(scores)) =>
            clearAnnouncement()
            signals.updateTotalScores(scores)
          case Right(PlayersOnPauseMessage(_)) =>
          // one or more players are having connection troubles
          // TODO: notify user that she needs to wait until all players are back
          case Right(TableIsFull) =>
            announce("Sorry, no more space at the table")
            nameInputHidden.set(true)
        }
    })

    renderOnDomContentLoaded("#namearea", StringComponents.nameInput(nameInputHidden, socket.write))
    renderOnDomContentLoaded(
      "#announcements",
      StringComponents.announcement(announcement.toObservable)
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
      "#povertybuttons",
      Buttons.povertyOnOfferButtons(signals.povertyOffered, actionSink)
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

  private object GameStateHandlers {

    def handleGameState(gameState: GameState, actionSink: PlayerAction[GameState] => Unit): Unit = {
      signals.updateGameState(gameState)
      gameState match {
        case w: WaitingForReservations =>
          waitingForReservations(w)
        case r: ReservationResult =>
          reservationResult(r)
        case p: PovertyOnOffer =>
          povertyOnOffer(p)
        case _: PovertyRefused =>
          povertyRefused()
        case p: PovertyExchange =>
          povertyExchange(p)
        case _: AskingForReservations | _: Playing | _: RoundResults =>
      }
    }

    private def waitingForReservations(
      state: WaitingForReservations
    ): Unit = {
      announce(ReservationStrings.default.toString(state.ownReservation))
    }

    private def povertyOnOffer(
      state: PovertyOnOffer
    ): Unit = {
      val txt = {
        val whom = if (state.playerIsBeingAsked) "Dir" else "jemandem"
        s"${playerName(state.playerOffering)} bietet $whom eine ${state.sizeOfPoverty}er-Armut an."
      }
      announce(txt)

    }

    private def povertyRefused(): Unit = {
      announce("Die Armut wurde nicht angenommen")
    }

    private def povertyExchange(
      state: PovertyExchange
    ): Unit = {
      val txt = {
        state.role match {
          case Accepting =>
            s"Wähle die Karten, die du ${playerName(state.playerOffering)} zurück geben willst"
          case Poor =>
            s"${playerName(state.playerAccepting)} guckt sich deine Karten an."
          case NotInvolved =>
            s"${playerName(state.playerAccepting)} guckt sich die Karten von ${playerName(state.playerOffering)} an."
        }
      }
      announce(txt)

    }

    private def reservationResult(
      state: ReservationResult
    ): Unit = {
      val txt = state.result.fold(ReservationStrings.default.toString(None)) { case (pos, r) =>
        s"${playerName(pos)}: ${ReservationStrings.default.toString(Some(r))}"
      }
      announce(txt)
    }

  }

}

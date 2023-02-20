package io.github.mahh.doko.client

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.EventStream
import com.raquo.laminar.api.L.Signal
import com.raquo.laminar.api.L.Var
import com.raquo.laminar.api.L.windowEvents
import com.raquo.laminar.nodes
import io.github.mahh.doko.client.ElementFactory.*
import io.github.mahh.doko.client.components.*
import io.github.mahh.doko.client.state.Signals
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.AskingForReservations
import io.github.mahh.doko.shared.game.GameState.Playing
import io.github.mahh.doko.shared.game.GameState.PovertyExchange
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.Accepting
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.NotInvolved
import io.github.mahh.doko.shared.game.GameState.PovertyExchange.Poor
import io.github.mahh.doko.shared.game.GameState.PovertyOnOffer
import io.github.mahh.doko.shared.game.GameState.PovertyRefused
import io.github.mahh.doko.shared.game.GameState.ReservationResult
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.game.GameState.WaitingForReservations
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
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.TotalScores
import org.scalajs.dom
import org.scalajs.dom.*

/**
 * The client's main code.
 */
object Client {

  @inline private def renderOnDomContentLoaded(
    selectors: => String,
    rootNode: => nodes.ReactiveElement.Base
  ): Unit = L.renderOnDomContentLoaded(
    dom.document.querySelector(selectors),
    rootNode
  )

  private def elementById[E <: Element](elementId: String): E = {
    dom.document.getElementById(elementId).asInstanceOf[E]
  }

  private val playground: HTMLDivElement = elementById("playground")
  private val autoOkCheckBox: HTMLInputElement = elementById("auto-ok")

  private def playerName(pos: PlayerPosition): String =
    signals.playerName(pos)

  // Handling of automatic acknowledgments via timer
  private val DefaultWait = 5
  private val ResultsWait = 15
  private val acknowledgeCountDown = new ActionCountDown(autoOkCheckBox.checked, DefaultWait)

  private val signals = new Signals

  private val nameInputHidden = Var(true)

  private val announcement = Var(Option.empty[String])

  private val trick = Var(Map.empty[PlayerPosition, CardConfig])
  private val hand = Var(Seq.empty[CardConfig])

  private def announce(msg: String): Unit = announcement.set(Some(msg))
  private def clearAnnouncement(): Unit = announcement.set(None)

  def main(args: Array[String]): Unit = {

    announce("Joining...")

    val socket: Socket = new Socket

    def actionSink(action: PlayerAction[GameState]): Unit = {
      socket.write(PlayerActionMessage(action))
      acknowledgeCountDown.clear()
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
      "#bidbuttons",
      Buttons.bidButtons(signals.bidsConfig, b => actionSink(PlayerAction.PlaceBid(b)))
    )
    renderOnDomContentLoaded("#trick", Cards.trick(trick.toObservable))
    renderOnDomContentLoaded("#hand", Cards.hand(hand.toObservable))
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
  }

  private object GameStateHandlers {

    private def withCleanPlayground[T](action: => T): T = {
      playground.innerHTML = ""
      action
    }

    def handleGameState(gameState: GameState, actionSink: PlayerAction[GameState] => Unit): Unit = {
      signals.updateGameState(gameState)
      gameState match {
        case r: AskingForReservations =>
          askingForReservations(r)
        case w: WaitingForReservations =>
          waitingForReservations(w)
        case r: ReservationResult =>
          reservationResult(r, actionSink)
        case p: PovertyOnOffer =>
          povertyOnOffer(p, actionSink)
        case _: PovertyRefused =>
          povertyRefused(actionSink)
        case p: PovertyExchange =>
          povertyExchange(p, actionSink)
        case r: Playing =>
          playing(r, actionSink)
        case r: RoundResults =>
          roundResult(r, actionSink)
      }
    }

    private def askingForReservations(
      state: AskingForReservations
    ): Unit = withCleanPlayground {
      updateTrick(Map.empty)
      updateHand(state.hand)
    }

    private def waitingForReservations(
      state: WaitingForReservations
    ): Unit = withCleanPlayground {
      updateHand(state.hand)

      playground.appendChild(p(ReservationStrings.default.toString(state.ownReservation)))
    }

    private def povertyOnOffer(
      state: PovertyOnOffer,
      actionSink: PlayerAction[PovertyOnOffer] => Unit
    ): Unit = withCleanPlayground {
      updateHand(state.hand)

      val txt = {
        val whom = if (state.playerIsBeingAsked) "Dir" else "jemandem"
        s"${playerName(state.playerOffering)} bietet $whom eine ${state.sizeOfPoverty}er-Armut an."
      }
      playground.appendChild(p(txt))

      if (state.playerIsBeingAsked) {
        playground.appendChild(
          buttonElement("Annehmen", () => actionSink(PlayerAction.PovertyReply(true)))
        )
        playground.appendChild(
          buttonElement("Ablehnen", () => actionSink(PlayerAction.PovertyReply(false)))
        )
      }

    }

    private def povertyRefused(
      actionSink: PlayerAction[PovertyRefused] => Unit
    ): Unit = withCleanPlayground {
      announce("Die Armut wurde nicht angenommen")
      val acknowledge = () => actionSink(PlayerAction.AcknowledgePovertyRefused)
      acknowledgeCountDown.startCountdown(acknowledge)
      playground.appendChild(okButton(acknowledge))
    }

    private def povertyExchange(
      state: PovertyExchange,
      actionSink: PlayerAction[PovertyExchange] => Unit
    ): Unit = withCleanPlayground {
      val selected: Seq[Card] = state.playerState.fold(Seq.empty)(_.selected)
      val isAccepting = state.role == Accepting
      val handler: Card => Unit = card =>
        if (isAccepting && selected.size < state.sizeOfPoverty) {
          actionSink(PlayerAction.PovertySelect(card))
        }

      updateHand(state.hand, handler)

      if (selected.nonEmpty) {
        // reuse the "trick area" to display selected cards
        val cardMap = PlayerPosition.All.zip(selected).toMap
        val cardAction: PlayerPosition => () => Unit = p =>
          cardMap.get(p).fold(NoopCallback) { card => () =>
            actionSink(PlayerAction.PovertyDeselect(card))
          }
        updateTrick(cardMap, cardAction)
      }

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
      playground.appendChild(p(txt))

      if (isAccepting && selected.size == state.sizeOfPoverty) {
        val acknowledge: () => Unit = () => actionSink(PlayerAction.PovertyReturn)
        playground.appendChild(okButton(acknowledge, withCountDown = false))
      }
    }

    private def reservationResult(
      state: ReservationResult,
      actionSink: PlayerAction[ReservationResult] => Unit
    ): Unit = withCleanPlayground {
      updateHand(state.hand)

      val txt = state.result.fold(ReservationStrings.default.toString(None)) { case (pos, r) =>
        s"${playerName(pos)}: ${ReservationStrings.default.toString(Some(r))}"
      }
      playground.appendChild(p(txt))
      val acknowledge = () => actionSink(PlayerAction.AcknowledgeReservation)
      acknowledgeCountDown.startCountdown(acknowledge)
      playground.appendChild(okButton(acknowledge))
    }

    private def playing(
      state: Playing,
      actionSink: PlayerAction[Playing] => Unit
    ): Unit = withCleanPlayground {

      val acknowledgeOpt: Option[() => Unit] = {
        val needsAcknowledgment = state.playerState.exists(_.needsAck)
        if (needsAcknowledgment)
          Some(() => actionSink(PlayerAction.AcknowledgeTrickResult))
        else
          None
      }

      updateTrick(state.currentTrick.cards, _ => acknowledgeOpt.getOrElse(NoopCallback))

      updateHand(
        state.hand,
        card => if (state.canPlay(card)) actionSink(PlayerAction.PlayCard(card))
      )

      acknowledgeOpt.foreach { acknowledge =>
        acknowledgeCountDown.startCountdown(acknowledge)
        playground.appendChild(p(""))
        playground.appendChild(okButton(acknowledge))
      }
    }

    private def roundResult(
      state: RoundResults,
      actionSink: PlayerAction[RoundResults] => Unit
    ): Unit = withCleanPlayground {
      val acknowledge = () => actionSink(PlayerAction.AcknowledgeRoundResult)
      acknowledgeCountDown.startCountdown(acknowledge, ResultsWait)
      playground.appendChild(okButton(acknowledge))
    }

  }

  private def updateHand(cards: Seq[Card], onClick: Card => Unit = _ => ()): Unit = {
    hand.set(cards.map(c => CardConfig(c, () => onClick(c))))
  }

  private def updateTrick(
    cards: Map[PlayerPosition, Card],
    cardAction: PlayerPosition => () => Unit = _ => NoopCallback
  ): Unit = {
    trick.set(cards.map { case (p, c) =>
      p -> CardConfig(c, cardAction(p))
    })
  }

  private def okButton(onClick: () => Unit, withCountDown: Boolean = true): HTMLInputElement = {
    val actionCountDownOpt =
      if (withCountDown && autoOkCheckBox.checked) Some(acknowledgeCountDown) else None
    buttonElement("OK", onClick, actionCountDownOpt)
  }

}

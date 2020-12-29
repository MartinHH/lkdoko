package io.github.mahh.doko.client

import io.circe
import io.github.mahh.doko.client.ElementFactory._
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.client.strings.ScoreStrings
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.CardsPerPlayer
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
import org.scalajs.dom.raw._

/**
 * The client's main code.
 */
object Client {

  protected def getInstance(): this.type = this

  private def elementById[E <: Element](elementId: String): E = {
    dom.document.getElementById(elementId).asInstanceOf[E]
  }

  private val playground: HTMLDivElement = elementById("playground")

  private val nameField: HTMLInputElement = elementById("name")
  private val nameButton: HTMLInputElement = elementById("confirmname")
  private val autoOkCheckBox: HTMLInputElement = elementById("auto-ok")

  private var playerNames: Map[PlayerPosition, String] = Map.empty

  private def playerName(pos: PlayerPosition): String = playerNames.getOrElse(pos, pos.toString)

  // Handling of automatic acknowledgments via timer
  private val DefaultWait = 5
  private val ResultsWait = 15
  private val acknowledgeCountDown = new ActionCountDown(autoOkCheckBox.checked, DefaultWait)

  private def cardHeight: Int = (dom.window.innerWidth / 12.0).toInt

  def main(args: Array[String]): Unit = {
    writeToArea("Joining...")
    nameField.disabled = true
    nameButton.disabled = true

    val socket: Socket = new Socket

    def actionSink(action: PlayerAction[GameState]): Unit = {
      socket.write(PlayerActionMessage(action))
      acknowledgeCountDown.clear()
    }

    socket.setListener(new Socket.Listener {
      override def onOpen(isReconnect: Boolean): Unit = {
        if (!isReconnect) {
          writeToArea("Connection was successful!")
          nameField.disabled = false
          nameButton.disabled = nameField.value.isEmpty
          nameField.focus()
        }
      }

      override def onError(msg: String): Unit = {
        writeToArea(s"Failed: $msg")
      }

      override def onUpdate(update: Either[circe.Error, MessageToClient]): Unit = update match {
        case Left(error) =>
          writeToArea(s"Error reading message from server: $error")
        case Right(Joining) =>
          writeToArea("Waiting for others to join...")
        case Right(GameStateMessage(gameState)) =>
          GameStateHandlers.handleGameState(gameState, actionSink)
        case Right(PlayersMessage(players)) =>
          handlePlayersUpdate(players)
        case Right(TotalScoresMessage(scores)) =>
          println("Scores updated")
          handleTotalScoresUpdate(scores)
        case Right(PlayersOnPauseMessage(_)) =>
          // one or more players are having connection troubles
          // TODO: notify user that she needs to wait until all players are back
        case Right(TableIsFull) =>
          writeToArea("Sorry, no more space at the table")
          nameField.disabled = true
          socket.close()
      }
    })

    nameButton.onclick = { event: MouseEvent =>

      val txt = nameField.value
      if (txt.nonEmpty) {
        socket.write(SetUserName(txt))
      }
      event.preventDefault()
    }


    nameField.onkeypress = { event: KeyboardEvent =>
      val isValid = nameField.value.nonEmpty
      nameButton.disabled = !isValid
      if (isValid && event.keyCode == 13) {
        nameButton.click()
        event.preventDefault()
      }
    }
  }


  private object GameStateHandlers {

    private def withCleanPlayground[T](action: => T): T = {
      playground.innerHTML = ""
      action
    }

    def handleGameState(gameState: GameState, actionSink: PlayerAction[GameState] => Unit): Unit = {
      gameState match {
        case r: AskingForReservations =>
          askingForReservations(r, actionSink)
        case w: WaitingForReservations =>
          waitingForReservations(w)
        case r: ReservationResult =>
          reservationResult(r, actionSink)
        case p: PovertyOnOffer =>
          povertyOnOffer(p, actionSink)
        case PovertyRefused =>
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
      state: AskingForReservations,
      actionSink: PlayerAction[AskingForReservations] => Unit
    ): Unit = withCleanPlayground {
      drawBids(Map.empty)
      drawTrick(Map.empty)
      PlayerMarkers.markActivePlayer(None)

      val cards: HTMLDivElement = handElement(state.hand, cardHeight = cardHeight)
      playground.appendChild(cards)

      playground.appendChild(p(""))

      def appendButton(r: Option[Reservation]): Unit = {
        val button =
          buttonElement(ReservationStrings.default.toString(r), () => actionSink(PlayerAction.CallReservation(r)))
        playground.appendChild(button)
      }

      appendButton(None)
      state.possibleReservations.foreach(r => appendButton(Some(r)))

    }

    private def waitingForReservations(
      state: WaitingForReservations
    ): Unit = withCleanPlayground {
      val cards: HTMLDivElement = handElement(state.hand, cardHeight = cardHeight)
      playground.appendChild(cards)

      playground.appendChild(p(ReservationStrings.default.toString(state.ownReservation)))
    }

    private def povertyOnOffer(
      state: PovertyOnOffer,
      actionSink: PlayerAction[PovertyOnOffer] => Unit
    ): Unit = withCleanPlayground {
      val cards: HTMLDivElement = handElement(state.hand, cardHeight = cardHeight)
      playground.appendChild(cards)

      val txt = {
        val whom = if (state.playerIsBeingAsked) "Dir" else "jemandem"
        s"${playerName(state.playerOffering)} bietet $whom eine ${state.sizeOfPoverty}er-Armut an."
      }
      playground.appendChild(p(txt))

      if (state.playerIsBeingAsked) {
        playground.appendChild(buttonElement("Annehmen", () => actionSink(PlayerAction.PovertyReply(true))))
        playground.appendChild(buttonElement("Ablehnen", () => actionSink(PlayerAction.PovertyReply(false))))
      }

    }

    private def povertyRefused(
      actionSink: PlayerAction[PovertyRefused.type] => Unit
    ): Unit = withCleanPlayground {
      writeToArea("Die Armut wurde nicht angenommen")
      val acknowledge = () => actionSink(PlayerAction.AcknowledgePovertyRefused)
      acknowledgeCountDown.startCountdown(acknowledge)
      playground.appendChild(okButton(acknowledge))
    }

    // TODO(?): this does a lot of client-local state handling (via recursion) while usually, each
    //  user-click immediately triggers a PLayerAction being sent to the server. Maybe the whole
    //  state-handling for selection of "cards to return" should also be moved to the server?
    private def povertyExchange(
      state: PovertyExchange,
      actionSink: PlayerAction[PovertyExchange] => Unit,
      selected: Seq[Card] = Seq()
    ): Unit = withCleanPlayground {
      val isAccepting = state.role == Accepting
      val cardsInHand = state.hand diff selected

      val cards: HTMLDivElement = {
        val handler: Card => Option[() => Unit] =
          if (isAccepting && cardsInHand.size > CardsPerPlayer) {
            card => Some(() => povertyExchange(state, actionSink, selected :+ card))
          } else {
            _ => None
          }
        handElement(cardsInHand, handler, cardHeight = cardHeight)
      }
      playground.appendChild(cards)

      if (selected.nonEmpty) {
        // reuse the "trick area" to display selected cards
        drawCardsInTrickArea(selected, card => povertyExchange(state, actionSink, selected diff Seq(card)))
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

      if (isAccepting && cardsInHand.size == CardsPerPlayer) {
        val acknowledge: () => Unit = () => actionSink(PlayerAction.PovertyReturned(selected))
        playground.appendChild(okButton(acknowledge, withCountDown = false))
      }
    }

    private def reservationResult(
      state: ReservationResult,
      actionSink: PlayerAction[ReservationResult] => Unit
    ): Unit = withCleanPlayground {
      val cards: HTMLDivElement = handElement(state.hand, cardHeight = cardHeight)
      playground.appendChild(cards)

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
        val needsAcknowledgment = state.trickWinner.exists { case (_, unacknowledged) => unacknowledged }
        if (needsAcknowledgment)
          Some(() => actionSink(PlayerAction.AcknowledgeTrickResult))
        else
          None
      }

      drawBids(state.bids)
      drawTrick(state.currentTrick.cards, _ => acknowledgeOpt)

      state.trickWinner.fold[Unit] {
        PlayerMarkers.markActivePlayer(state.currentTrick.currentPlayer)
      } { case (pos, _) =>
        PlayerMarkers.markTrickWinner(pos)
      }

      val cards: HTMLDivElement = handElement(
        state.hand,
        c => if (state.canPlay(c)) Some(() => actionSink(PlayerAction.PlayCard(c))) else None,
        cardHeight = cardHeight
      )
      playground.appendChild(cards)

      showTrickCount(state.trickCounts)

      def appendButton(bid: NameableBid): Unit = {
        val button =
          buttonElement(BidStrings.default.toString(bid), () => actionSink(PlayerAction.PlaceBid(bid.bid)))
        playground.appendChild(button)
      }

      val possibleBids = state.possibleBid.fold[List[NameableBid]](List.empty) { case NameableBid(isElders, bid) =>
        Bid.All.filter(Bid.ordering.gteq(_, bid)).map(NameableBid(isElders, _))
      }

      possibleBids.foreach(appendButton)

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

      val results = state.scores.all

      val table: HTMLTableSectionElement = createElement("section")

      val players = tableRowDiv
      val values = tableRowDiv
      val scores = tableRowDiv
      val totals = tableRowDiv

      results.foreach { r =>
        players.appendCell(r.team.map(playerName).mkString("<br>"))
        values.appendCell(r.tricksValue.toString)
        def scoreString(s: Score): String = s"${ScoreStrings.default.toString(s)}: ${s.value}"
        scores.appendCell(r.scores.map(scoreString).mkString("<br>"))
      }

      state.scores.totals.foreach { total =>
        totals.appendCell(total.toString)
      }

      List(players, values, scores, totals).foreach(table.appendChild)

      playground.appendChild(table)

      val acknowledge = () => actionSink(PlayerAction.AcknowledgeRoundResult)
      acknowledgeCountDown.startCountdown(acknowledge, ResultsWait)
      playground.appendChild(okButton(acknowledge))

    }

  }

  private def handlePlayersUpdate(players: Map[PlayerPosition, String]): Unit = {
    playerNames = players
    PlayerPosition.All.foreach { pos =>
      val name = playerName(pos)

      def setNameInCell(cellIdPrefix: String): Unit = {
        val cellId = s"$cellIdPrefix${PlayerPosition.indexOf(pos)}"
        elementById[Element](cellId).innerHTML = name
      }

      setNameInCell("name")
      setNameInCell("scorename")
    }
  }

  private object PlayerMarkers {
    private def markPlayer(player: Option[PlayerPosition], marker: String): Unit = {
      PlayerPosition.All.foreach { pos =>
        val cellId = s"marker${PlayerPosition.indexOf(pos)}"
        val content = if (player.contains(pos)) marker else ""
        elementById[Element](cellId).innerHTML = content
      }
    }

    def markActivePlayer(player: Option[PlayerPosition]): Unit = {
      markPlayer(player, "^")
    }

    def markTrickWinner(player: PlayerPosition): Unit = {
      markPlayer(Some(player), "*")
    }
  }


  private def updateTableRow[T](
    contentMap: Map[PlayerPosition, T],
    default: => String,
    toString: T => String,
    cellIDPrefix: String
  ): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"$cellIDPrefix${PlayerPosition.indexOf(pos)}"
      val content = contentMap.get(pos).fold(default)(toString)
      elementById[Element](cellId).innerHTML = content
    }
  }

  private def updateTableRow(contentMap: Map[PlayerPosition, Int], cellIDPrefix: String): Unit = {
    updateTableRow[Int](contentMap, default = "0", _.toString, cellIDPrefix)
  }

  private def handleTotalScoresUpdate(scores: TotalScores): Unit = {
    val scoresMap: Map[PlayerPosition, Int] = scores.sumPerPlayer
    updateTableRow(scoresMap, "scorevalue")
  }

  private def showTrickCount(counts: Map[PlayerPosition, Int]): Unit = {
    updateTableRow(counts, "count")
  }

  private def drawTrick(
    cards: Map[PlayerPosition, Card],
    cardAction: PlayerPosition => Option[() => Unit] = _ => None
  ): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"card${PlayerPosition.indexOf(pos)}"
      val cell: Element = elementById(cellId)
      cell.innerHTML = ""
      cards.get(pos).foreach { card =>
        cell.appendChild(cardElement(card, cardAction(pos), cardHeight))
      }
    }
  }

  private def drawCardsInTrickArea(
    cards: Seq[Card],
    cardAction: Card => Unit
  ): Unit = {
    val cardMap = PlayerPosition.All.zip(cards).toMap
    drawTrick(cardMap, cardMap.get(_).map(card => () => cardAction(card)))
  }

  private def drawBids(bids: Map[PlayerPosition, NameableBid]): Unit = {
    updateTableRow(bids, default = "", BidStrings.default.summaryString, "bid")
  }

  private def writeToArea(text: String): Unit =
    playground.innerHTML = text

  private def okButton(onClick: () => Unit, withCountDown: Boolean = true): HTMLInputElement = {
    val actionCountDownOpt =
      if (withCountDown && autoOkCheckBox.checked) Some(acknowledgeCountDown) else None
    buttonElement("OK", onClick, actionCountDownOpt)
  }

  private implicit class RichTableCellDiv(private val elem: HTMLDivElement) extends AnyVal {
    def appendCell(content: String): Unit = {
      elem.appendChild(tableCellDiv(content))
    }
  }

}

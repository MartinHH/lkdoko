package io.github.mahh.doko.client

import io.circe
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.client.strings.ReservationStrings
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.AskingForReservations
import io.github.mahh.doko.shared.game.GameState.Playing
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
import io.github.mahh.doko.shared.score.TotalScores
import org.scalajs.dom
import org.scalajs.dom.raw._

import scala.concurrent.duration.DurationInt
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetIntervalHandle

object Client {

  protected def getInstance(): this.type = this

  private def elementById[E <: Element](elementId: String): E = {
    dom.document.getElementById(elementId).asInstanceOf[E]
  }

  private def createElement[E <: Element](tagName: String): E = {
    dom.document.createElement(tagName).asInstanceOf[E]
  }

  private val playground: HTMLDivElement = elementById("playground")

  private val nameField: HTMLInputElement = elementById("name")
  private val nameButton: HTMLInputElement = elementById("confirmname")
  private val autoOkCheckBox: HTMLInputElement = elementById("auto-ok")

  private var playerNames: Map[PlayerPosition, String] = Map.empty

  private def playerName(pos: PlayerPosition): String = playerNames.getOrElse(pos, pos.toString)

  /** Handling of automatic acknowledgments via timer. */
  private object AutoOk {
    private var timeoutHandle: Option[SetIntervalHandle] = None
    private var countDownCallbacks: Set[Int => Unit] = Set.empty
    private var countDown = 0

    // by default, we wait 5 seconds
    val DefaultWait = 5
    // for the results of a round, we wait 15 seconds
    val ResultsWait = 15

    def clear(): Unit = {
      timeoutHandle.foreach(timers.clearInterval)
      countDownCallbacks = Set.empty
      countDown = 0
    }

    def startCountdown(
      onFinished: () => Unit,
      seconds: Int = DefaultWait
    ): Unit = if (autoOkCheckBox.checked) {
      countDown = seconds

      val handle = {
        timers.setInterval(1.second) {
          countDown -= 1
          if (countDown <= 0) {
            if (autoOkCheckBox.checked) {
              onFinished()
            }
          } else {
            countDownCallbacks.foreach(_.apply(countDown))
          }
        }
      }
      timeoutHandle = Some(handle)
    }

    def addCountDownCallback(callback: Int => Unit): Unit = {
      countDownCallbacks += callback
      if (timeoutHandle.nonEmpty) {
        callback(countDown)
      }
    }
  }


  def cardHeight: Int = (dom.window.innerWidth / 12.0).toInt

  def main(args: Array[String]): Unit = {
    playground.innerHTML = s"Joining..."
    nameField.disabled = true
    nameButton.disabled = true

    val socket: Socket = new Socket

    def actionSink(action: PlayerAction[GameState]): Unit = {
      socket.write(PlayerActionMessage(action))
      AutoOk.clear()
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
          handleGameState(gameState, actionSink)
        case Right(PlayersMessage(players)) =>
          handlePlayersUpdate(players)
        case Right(TotalScoresMessage(scores)) =>
          println("Scores updated")
          handleTotalScoresUpdate(scores)
        case Right(PlayersOnPauseMessage(_)) =>
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


  def handleGameState(gameState: GameState, actionSink: PlayerAction[GameState] => Unit): Unit =
    gameState match {
      case r: AskingForReservations =>
        handleAskingForReservations(r, actionSink)
      case w: WaitingForReservations =>
        handleWaitingForReservations(w)
      case r: ReservationResult =>
        handleReservationResult(r, actionSink)
      case p: PovertyOnOffer =>
        handlePovertyOnOffer(p, actionSink)
      case PovertyRefused =>
        handlePovertyRefused(actionSink)
      case r: Playing =>
        handlePlaying(r, actionSink)
      case r: RoundResults =>
        handleRoundResult(r, actionSink)
      case x =>
        writeToArea(x.toString)
    }


  def handleAskingForReservations(
    state: AskingForReservations,
    actionSink: PlayerAction[AskingForReservations] => Unit
  ): Unit = {
    playground.innerHTML = ""
    drawTrick(Map.empty)
    markActivePlayer(None)

    val cards: HTMLDivElement = handElement(state.hand)
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

  def handleWaitingForReservations(
    state: WaitingForReservations
  ): Unit = {
    playground.innerHTML = ""
    val cards: HTMLDivElement = handElement(state.hand)
    playground.appendChild(cards)

    playground.appendChild(p(ReservationStrings.default.toString(state.ownReservation)))
  }

  def handlePovertyOnOffer(
    state: PovertyOnOffer,
    actionSink: PlayerAction[PovertyOnOffer] => Unit
  ): Unit = {
    playground.innerHTML = ""
    val cards: HTMLDivElement = handElement(state.hand)
    playground.appendChild(cards)

    val txt = {
      val whom = if (state.playerIsBeingAsked) "Dir" else "jemandem"
      s"${playerName(state.playerOffering)} bietet $whom eine ${state.sizeOfPoverty}er-Armut an."
    }
    playground.appendChild(p(txt))

    if (state.playerIsBeingAsked) {
      playground.appendChild(buttonElement("Annehmen", () => actionSink(PlayerAction.PovertyReply(true))))
      playground.appendChild(buttonElement("Ablehnen", () => actionSink(PlayerAction.PovertyReply(true))))
    }

  }

  def handlePovertyRefused(
    actionSink: PlayerAction[PovertyRefused.type] => Unit
  ): Unit = {
    playground.innerHTML = ""
    writeToArea("Die Armut wurde nicht angenommen")
    val acknowledge = () => actionSink(PlayerAction.AcknowledgePovertyRefused)
    AutoOk.startCountdown(acknowledge)
    playground.appendChild(okButton(acknowledge))
  }

  def handleReservationResult(
    state: ReservationResult,
    actionSink: PlayerAction[ReservationResult] => Unit
  ): Unit = {
    playground.innerHTML = ""
    val cards: HTMLDivElement = handElement(state.hand)
    playground.appendChild(cards)

    val txt = state.result.fold(ReservationStrings.default.toString(None)) { case (pos, r) =>
      s"${playerName(pos)}: ${ReservationStrings.default.toString(Some(r))}"
    }
    playground.appendChild(p(txt))
    val acknowledge = () => actionSink(PlayerAction.AcknowledgeReservation)
    AutoOk.startCountdown(acknowledge)
    playground.appendChild(okButton(acknowledge))
  }

  def handlePlaying(
    state: Playing,
    actionSink: PlayerAction[Playing] => Unit
  ): Unit = {

    val needsAcknowledgment = state.trickWinner.exists { case (_, unacknowledged) => unacknowledged }

    val acknowledgeOpt: Option[() => Unit] =
      if (needsAcknowledgment)
        Some(() => actionSink(PlayerAction.AcknowledgeTrickResult))
      else
        None

    drawBids(state.bids)
    drawTrick(state.currentTrick.cards, _ => acknowledgeOpt)

    state.trickWinner.fold[Unit] {
      markActivePlayer(state.currentTrick.currentPlayer)
    } { case (pos, _) =>
      markTrickWinner(pos)
    }


    playground.innerHTML = ""
    val cards: HTMLDivElement = handElement(
      state.hand,
      c => if (state.canPlay(c)) Some(() => actionSink(PlayerAction.PlayCard(c))) else None
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
      AutoOk.startCountdown(acknowledge)
      playground.appendChild(p(""))
      playground.appendChild(okButton(acknowledge))
    }
  }

  def handleRoundResult(
    state: RoundResults,
    actionSink: PlayerAction[RoundResults] => Unit
  ): Unit = {

    val results = state.scores.all

    playground.innerHTML = ""
    val table: HTMLTableSectionElement = createElement("section")

    val players = tableRowDiv
    val values = tableRowDiv
    val scores = tableRowDiv
    val totals = tableRowDiv

    results.foreach { r =>
      players.appendCell(r.team.map(playerName).mkString("<br>"))
      values.appendCell(r.tricksValue.toString)
      scores.appendCell(r.scores.map(s => s"$s: ${s.value}").mkString("<br>"))
    }

    state.scores.totals.foreach { total =>
      totals.appendCell(total.toString)
    }

    List(players, values, scores, totals).foreach(table.appendChild)

    playground.appendChild(table)

    val acknowledge = () => actionSink(PlayerAction.AcknowledgeRoundResult)
    AutoOk.startCountdown(acknowledge, AutoOk.ResultsWait)
    playground.appendChild(okButton(acknowledge))

  }

  def handlePlayersUpdate(players: Map[PlayerPosition, String]): Unit = {
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

  def markPlayer(player: Option[PlayerPosition], marker: String): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"marker${PlayerPosition.indexOf(pos)}"
      val content = if (player.contains(pos)) marker else ""
      elementById[Element](cellId).innerHTML = content
    }
  }

  def markActivePlayer(player: Option[PlayerPosition]): Unit = {
    markPlayer(player, "^")
  }

  private def updateTableRow(contentMap: Map[PlayerPosition, Int], cellIDPrefix: String): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"$cellIDPrefix${PlayerPosition.indexOf(pos)}"
      val content = contentMap.getOrElse(pos, 0).toString
      elementById[Element](cellId).innerHTML = content
    }
  }

  def handleTotalScoresUpdate(scores: TotalScores): Unit = {
    val scoresMap: Map[PlayerPosition, Int] = scores.sumPerPlayer
    updateTableRow(scoresMap, "scorevalue")
  }

  def markTrickWinner(player: PlayerPosition): Unit = {
    markPlayer(Some(player), "*")
  }

  def showTrickCount(counts: Map[PlayerPosition, Int]): Unit = {
    updateTableRow(counts, "count")
  }

  def drawTrick(
    cards: Map[PlayerPosition, Card],
    cardAction: PlayerPosition => Option[() => Unit] = _ => None
  ): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"card${PlayerPosition.indexOf(pos)}"
      val cell: Element = elementById(cellId)
      cell.innerHTML = ""
      cards.get(pos).foreach { card =>
        cell.appendChild(cardElement(card, cardAction(pos)))
      }
    }
  }

  def drawBids(
    bids: Map[PlayerPosition, NameableBid]
  ): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"bid${PlayerPosition.indexOf(pos)}"
      val cell: Element = elementById(cellId)
      val bidString = bids.get(pos).fold("")(BidStrings.default.summaryString)
      cell.innerHTML = bidString
    }
  }

  def writeToArea(text: String): Unit =
    playground.innerHTML = text


  def p(msg: String): Element = {
    val paragraph: Element = createElement("p")
    paragraph.innerHTML = msg
    paragraph
  }

  def cardElement(card: Card, handler: Option[() => Unit]): HTMLImageElement = {
    val c: HTMLImageElement = createElement("img")
    val uri = SvgPaths.getSvgUri(card)
    c.src = uri
    c.textContent = SvgPaths.getSvgUri(card)
    c.height = cardHeight
    handler.foreach { h =>
      c.onclick = _ => h()
    }
    c
  }

  def handElement(
    hand: Iterable[Card],
    handler: Card => Option[() => Unit] = _ => None
  ): HTMLDivElement = {
    val cards: HTMLDivElement = createElement("div")
    hand.foreach { card =>
      cards.appendChild(cardElement(card, handler(card)))
    }
    cards
  }

  def okButton[A <: PlayerAction[GameState]](
    onClick: () => Unit
  ): HTMLInputElement = {
    buttonElement("OK", onClick, withCountDown = autoOkCheckBox.checked)
  }

  def buttonElement[A <: PlayerAction[GameState]](
    title: String,
    onClick: () => Unit,
    withCountDown: Boolean = false
  ): HTMLInputElement = {
    val button: HTMLInputElement = createElement("input")
    button.`type` = "button"
    button.value = title

    if (withCountDown) {
      AutoOk.addCountDownCallback { remaining =>
        button.value = s"$title ($remaining)"
      }
    }

    button.onclick = _ => {
      onClick()
    }
    button
  }

  def tableRowDiv: HTMLDivElement = {
    val div: HTMLDivElement = createElement("div")
    div.style = "display: table-row;"
    div
  }

  def tableCellDiv(content: String): HTMLDivElement = {
    val div: HTMLDivElement = createElement("div")
    div.style = "display: table-cell; padding: 0px 5px;"
    div.innerHTML = content
    div
  }

  implicit class RichTableCellDiv(private val elem: HTMLDivElement) extends AnyVal {
    def appendCell(content: String): Unit = {
      elem.appendChild(tableCellDiv(content))
    }
  }

}

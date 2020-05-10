package io.github.mahh.doko.client

import io.circe
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.AskingForReservations
import io.github.mahh.doko.shared.game.GameState.Joining
import io.github.mahh.doko.shared.game.GameState.Playing
import io.github.mahh.doko.shared.game.GameState.PovertyOnOffer
import io.github.mahh.doko.shared.game.GameState.PovertyRefused
import io.github.mahh.doko.shared.game.GameState.ReservationResult
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.game.GameState.WaitingForReservations
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.GameStateMessage
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

object Client {

  protected def getInstance(): this.type = this

  private val playground = dom.document.getElementById("playground").asInstanceOf[HTMLDivElement]

  private val nameField = dom.document.getElementById("name").asInstanceOf[HTMLInputElement]
  private val nameButton = dom.document.getElementById("confirmname").asInstanceOf[HTMLInputElement]

  private var playerNames: Map[PlayerPosition, String] = Map.empty

  private def playerName(pos: PlayerPosition): String = playerNames.getOrElse(pos, pos.toString)


  def cardHeight: Int = (dom.window.innerWidth / 12.0).toInt

  def main(args: Array[String]): Unit = {
    playground.innerHTML = s"Joining..."
    nameField.disabled = true
    nameButton.disabled = true

    val socket: Socket = new Socket

    socket.setListener(new Socket.Listener {
      override def onOpen(isReconnect: Boolean): Unit = {
        if (!isReconnect) {
          writeToArea("Connection was successful!")
          nameField.disabled = false
          nameButton.disabled = nameField.value.isEmpty
          nameField.focus()
          // TODO: disable on close
        }
      }

      override def onError(msg: String): Unit = {
        writeToArea(s"Failed: $msg")
      }

      override def onUpdate(update: Either[circe.Error, MessageToClient]): Unit = update match {
        case Left(error) =>
          writeToArea(s"Error reading message from server: $error")
        case Right(GameStateMessage(gameState)) =>
          handleGameState(gameState, a => socket.write(PlayerActionMessage(a)))
        case Right(PlayersMessage(players)) =>
          handlePlayersUpdate(players)
        case Right(TotalScoresMessage(scores)) =>
          println("Scores updated")
          handleTotalScoresUpdate(scores)
        case Right(PlayersOnPauseMessage(_)) =>
          // TODO: notify user that she needs to wait until all players are back
        case Right(TableIsFull) =>
          writeToArea("Sorry, no more space at the table")
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
      case Joining =>
        writeToArea("Waiting for others to join...")
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
        buttonElement(ReservationStrings.default.toString(r), PlayerAction.CallReservation(r), actionSink)
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
      playground.appendChild(buttonElement("Annehmen", PlayerAction.PovertyReply(true), actionSink))
      playground.appendChild(buttonElement("Ablehnen", PlayerAction.PovertyReply(false), actionSink))
    }

  }

  def handlePovertyRefused(
    actionSink: PlayerAction[PovertyRefused.type] => Unit
  ): Unit = {
    playground.innerHTML = ""
    writeToArea("Die Armut wurde nicht angenommen")
    playground.appendChild(buttonElement("OK", PlayerAction.AcknowledgePovertyRefused, actionSink))
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

    playground.appendChild(buttonElement("OK", PlayerAction.AcknowledgeReservation, actionSink))
  }

  def handlePlaying(
    state: Playing,
    actionSink: PlayerAction[Playing] => Unit
  ): Unit = {

    val needsAcknowledgment = state.trickWinner.exists { case (_, unacknowledged) => unacknowledged }

    val cardAction: PlayerPosition => Option[() => Unit] =
      if (needsAcknowledgment)
        _ => Some(() => actionSink(PlayerAction.AcknowledgeTrickResult))
      else
        _ => None

    drawTrick(state.currentTrick.cards, cardAction)

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
    // TODO: calls

    if (needsAcknowledgment) {
      playground.appendChild(buttonElement("OK", PlayerAction.AcknowledgeTrickResult, actionSink))
    }
  }

  def handleRoundResult(
    state: RoundResults,
    actionSink: PlayerAction[RoundResults] => Unit
  ): Unit = {

    val results = state.scores.all

    playground.innerHTML = ""
    val table = dom.document.createElement("section").asInstanceOf[HTMLTableSectionElement]

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

    playground.appendChild(buttonElement("OK", PlayerAction.AcknowledgeRoundResult, actionSink))

  }

  def handlePlayersUpdate(players: Map[PlayerPosition, String]): Unit = {
    playerNames = players
    PlayerPosition.All.foreach { pos =>
      val name = playerName(pos)
      // TODO: Make this less redundant
      val cellId = s"name${PlayerPosition.indexOf(pos)}"
      dom.document.getElementById(cellId).innerHTML = name
      val valueCellId = s"scorename${PlayerPosition.indexOf(pos)}"
      dom.document.getElementById(valueCellId).innerHTML = name
    }
  }

  def markPlayer(player: Option[PlayerPosition], marker: String): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"marker${PlayerPosition.indexOf(pos)}"
      val content = if (player.contains(pos)) marker else ""
      dom.document.getElementById(cellId).innerHTML = content
    }
  }

  def markActivePlayer(player: Option[PlayerPosition]): Unit = {
    markPlayer(player, "^")
  }

  def handleTotalScoresUpdate(scores: TotalScores): Unit = {
    val scoresMap = scores.sumPerPlayer
    PlayerPosition.All.foreach { pos =>
      val cellId = s"scorevalue${PlayerPosition.indexOf(pos)}"
      val content = scoresMap.getOrElse(pos, 0).toString
      dom.document.getElementById(cellId).innerHTML = content
    }
  }

  def markTrickWinner(player: PlayerPosition): Unit = {
    markPlayer(Some(player), "*")
  }

  def showTrickCount(counts: Map[PlayerPosition, Int]): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"count${PlayerPosition.indexOf(pos)}"
      val content = counts.getOrElse(pos, 0).toString
      dom.document.getElementById(cellId).innerHTML = content
    }
  }

  def drawTrick(
    cards: Map[PlayerPosition, Card],
    cardAction: PlayerPosition => Option[() => Unit] = _ => None
  ): Unit = {
    PlayerPosition.All.foreach { pos =>
      val cellId = s"card${PlayerPosition.indexOf(pos)}"
      val cell = dom.document.getElementById(cellId)
      cell.innerHTML = ""
      cards.get(pos).foreach { card =>
        cell.appendChild(cardElement(card, cardAction(pos)))
      }
    }
  }

  def writeToArea(text: String): Unit =
    playground.innerHTML = text


  def p(msg: String): Element = {
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = msg
    paragraph
  }

  def cardElement(card: Card, handler: Option[() => Unit]): HTMLImageElement = {
    val c = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    val uri = SvgPaths.getSvgUri(card)
    c.src = uri
    c.textContent = SvgPaths.getSvgUri(card)
    c.height = cardHeight
    handler.foreach { h =>
      c.onclick = _ => {
        h()
      }
    }
    c
  }

  def handElement(
    hand: Iterable[Card],
    handler: Card => Option[() => Unit] = _ => None
  ): HTMLDivElement = {
    val cards: HTMLDivElement = dom.document.createElement("div").asInstanceOf[HTMLDivElement]
    hand.foreach { card =>
      cards.appendChild(cardElement(card, handler(card)))
    }
    cards
  }

  def buttonElement[A <: PlayerAction[GameState]](
    title: String,
    action: A,
    actionSink: A => Unit
  ): HTMLInputElement = {
    val button = dom.document.createElement("input").asInstanceOf[HTMLInputElement]
    button.`type` = "button"
    button.value = title
    button.onclick = _ => {
      actionSink(action)
    }
    button
  }

  def tableRowDiv: HTMLDivElement = {
    val div = dom.document.createElement("div").asInstanceOf[HTMLDivElement]
    div.style = "display: table-row;"
    div
  }

  def tableCellDiv(content: String): HTMLDivElement = {
    val div = dom.document.createElement("div").asInstanceOf[HTMLDivElement]
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

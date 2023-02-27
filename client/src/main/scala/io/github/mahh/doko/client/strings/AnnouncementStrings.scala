package io.github.mahh.doko.client.strings

import io.github.mahh.doko.client.components.fullHeightEmptyString
import io.github.mahh.doko.client.state.ClientGameState
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.*
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToClient.Joining
import io.github.mahh.doko.shared.table.TableMap

object AnnouncementStrings:

  def forConnectionState(state: ClientGameState, playerNames: TableMap[String]): String =
    state match {
      case ClientGameState.Connecting =>
        "Joining..."
      case ClientGameState.Joining =>
        "Waiting for others to join..."
      case ClientGameState.GameInProgress(gs) =>
        forGameState(gs, playerNames)
      case ClientGameState.Error(msg) =>
        s"Error: $msg"
    }

  private def forGameState(state: GameState, playerNames: TableMap[String]): String = state match {
    case r: ReservationResult =>
      r.result.fold(ReservationStrings.default.toString(None)) { case (pos, r) =>
        s"${playerNames(pos)}: ${ReservationStrings.default.toString(Some(r))}"
      }
    case p: PovertyOnOffer =>
      val whom = if (p.playerIsBeingAsked) "Dir" else "jemandem"
      s"${playerNames(p.playerOffering)} bietet $whom eine ${p.sizeOfPoverty}er-Armut an."
    case _: PovertyRefused =>
      "Die Armut wurde nicht angenommen."
    case p: PovertyExchange =>
      p.role match {
        case PovertyExchange.Accepting =>
          s"Wähle die Karten, die du ${playerNames(p.playerOffering)} zurück geben willst"
        case PovertyExchange.Poor =>
          s"${playerNames(p.playerAccepting)} guckt sich deine Karten an."
        case PovertyExchange.NotInvolved =>
          s"${playerNames(p.playerAccepting)} guckt sich die Karten von ${playerNames(p.playerOffering)} an."
      }
    case gs if gs.playerState.isEmpty =>
      "Sorry, no more space at the table"
    case _ =>
      fullHeightEmptyString
  }

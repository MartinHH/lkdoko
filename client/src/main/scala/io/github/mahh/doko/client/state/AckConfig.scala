package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.*
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgePovertyRefused
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgeReservation
import io.github.mahh.doko.shared.player.PlayerAction.Acknowledgement
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgeTrickResult
import io.github.mahh.doko.shared.player.PlayerAction.AcknowledgeRoundResult
import io.github.mahh.doko.shared.player.PlayerAction.PovertyReturn

/**
 * Configuration of the "acknowledgment"-part of the UI.
 *
 * @param autoAckTimeout The timeout after which (if active) auto-acknowledgment should "fire".
 * @param ack The `Acknowledgement` that is due.
 */
case class AckConfig(autoAckTimeout: Option[Int], ack: PlayerAction[GameState])

object AckConfig:

  private val shortTimeout = 5
  private val longTimeout = 15

  def forState(state: GameState): Option[AckConfig] = {
    def short(ack: Acknowledgement[GameState]) = Some(AckConfig(Some(shortTimeout), ack))
    def long(ack: Acknowledgement[GameState]) = Some(AckConfig(Some(longTimeout), ack))
    state match {
      case p: PovertyRefused if p.playerState.exists(_.needsAck) =>
        short(AcknowledgePovertyRefused)
      case r: ReservationResult if r.playerState.exists(_.needsAck) =>
        short(AcknowledgeReservation)
      case p: Playing if p.playerState.exists(_.needsAck) =>
        short(AcknowledgeTrickResult)
      case r: RoundResults if r.playerState.exists(_.needsAck) =>
        long(AcknowledgeRoundResult)
      case _: AskingForReservations | _: WaitingForReservations | _: ReservationResult |
          _: PovertyOnOffer | _: PovertyRefused | _: PovertyExchange | _: Playing |
          _: RoundResults =>
        None
    }
  }

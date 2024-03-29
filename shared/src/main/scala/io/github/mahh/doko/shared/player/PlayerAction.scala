package io.github.mahh.doko.shared.player

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation

/**
 * An action that can be performed by a player.
 *
 * @tparam G Phase of the game during which the action can be performed.
 */
sealed trait PlayerAction[+G <: GameState]

object PlayerAction {

  case class CallReservation(reservationOpt: Option[Reservation])
    extends PlayerAction[GameState.AskingForReservations]

  case class PovertyReply(accepted: Boolean) extends PlayerAction[GameState.PovertyOnOffer]

  case class PovertySelect(selected: Card) extends PlayerAction[GameState.PovertyExchange]

  case class PovertyDeselect(selected: Card) extends PlayerAction[GameState.PovertyExchange]

  case object PovertyReturn extends PlayerAction[GameState.PovertyExchange]

  case class PlaceBid(bid: Bid) extends PlayerAction[GameState.Playing]

  case class PlayCard(card: Card) extends PlayerAction[GameState.Playing]

  /**
   * An action where a player confirms that the game can proceed.
   *
   * (These are used to ensure that each player has understood the current games state before proceeding.)
   */
  sealed trait Acknowledgement[+G <: GameState] extends PlayerAction[G]

  case object AcknowledgePovertyRefused extends Acknowledgement[GameState.PovertyRefused]

  case object AcknowledgeReservation extends Acknowledgement[GameState.ReservationResult]

  case object AcknowledgeTrickResult extends Acknowledgement[GameState.Playing]

  case object AcknowledgeRoundResult extends Acknowledgement[GameState.RoundResults]

}

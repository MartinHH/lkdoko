package io.github.mahh.doko.shared.game

import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.Reservation.LastingReservation
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Scores

/**
 * State of the game from a single player's perspective.
 */
sealed trait GameState

object GameState {


  /**
   * All players have joined, cards have been dealt and players may now call "reservations".
   *
   * @param hand                 The cards that have been dealt to the player.
   * @param possibleReservations The reservations the player is allowed to call.
   */
  case class AskingForReservations(
    hand: Seq[Card],
    possibleReservations: Seq[Reservation]
  ) extends GameState

  /**
   * Reservations have been made, players are presented the result.
   *
   * @param hand   The cards that have been dealt to the player.
   * @param result The "winning" reservation (if any).
   */
  case class ReservationResult(
    hand: Seq[Card],
    result: Option[(PlayerPosition, Reservation)]
  ) extends GameState

  /**
   * Player has answered to [[AskingForReservations]], but other players have not answered yet.
   *
   * @param hand           The cards that have been dealt to the player.
   * @param ownReservation The reservations the player has called (if any).
   */
  case class WaitingForReservations(
    hand: Seq[Card],
    ownReservation: Option[Reservation]
  ) extends GameState

  // TODO: Poverty...

  case class PovertyOnOffer(
    hand: Seq[Card],
    sizeOfPoverty: Int,
    playerOffering: PlayerPosition,
    playerIsBeingAsked: Boolean
  ) extends GameState

  case object PovertyRefused extends GameState

  /**
   * Game is in progress.
   *
   * @param hand              The cards of the player.
   * @param currentTrick      The trick that is being played.
   * @param bids              Any bids that were called so far.
   * @param reservation       The active reservation if any.
   * @param possibleBid       The bid the player could call next.
   * @param trickCounts       How many tricks each player has won so far
   * @param canPlay           The cards that the player if allowed to play.
   * @param trickWinner       The winner of the current trick.
   */
  case class Playing(
    hand: Seq[Card],
    currentTrick: Trick,
    bids: Map[PlayerPosition, NameableBid],
    reservation: Option[(PlayerPosition, LastingReservation)],
    possibleBid: Option[NameableBid],
    trickCounts: Map[PlayerPosition, Int],
    canPlay: Set[Card],
    trickWinner: Option[(PlayerPosition, Boolean)]
  ) extends GameState

  /**
   * End of the round - results of the round are presented to each player.
   * @param scores The results of the round.
   */
  case class RoundResults(
    scores: Scores
  ) extends GameState
}


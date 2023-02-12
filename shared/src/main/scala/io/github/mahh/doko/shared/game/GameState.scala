package io.github.mahh.doko.shared.game

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.Reservation.LastingReservation
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Scores

/**
 * State of the game from a single player's perspective.
 */
sealed trait GameState {

  type PlayerState

  val playerState: Option[PlayerState]

  protected def playerSeq[T](f: PlayerState => Seq[T]): Seq[T] = playerState.fold(Seq.empty[T])(f)
  protected def playerSet[T](f: PlayerState => Set[T]): Set[T] = playerState.fold(Set.empty[T])(f)
  protected def playerOpt[T](f: PlayerState => Option[T]): Option[T] = playerState.flatMap(f)

}

object GameState {

  /**
   * All players have joined, cards have been dealt and players may now call "reservations".
   */
  case class AskingForReservations(playerState: Option[AskingForReservations.PlayerState])
    extends GameState {
    override type PlayerState = AskingForReservations.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
    def possibleReservations: Seq[Reservation] = playerSeq(_.possibleReservations)
  }

  object AskingForReservations {

    /**
     * @param hand                 The cards that have been dealt to the player.
     * @param possibleReservations The reservations the player is allowed to call.
     */
    case class PlayerState(hand: Seq[Card], possibleReservations: Seq[Reservation])
  }

  /**
   * Reservations have been made, players are presented the result.
   *
   * @param result The "winning" reservation (if any).
   */
  case class ReservationResult(
    result: Option[(PlayerPosition, Reservation)],
    playerState: Option[ReservationResult.PlayerState]
  ) extends GameState {
    override type PlayerState = ReservationResult.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
  }

  object ReservationResult {

    /**
     * @param hand                 The cards that have been dealt to the player.
     */
    case class PlayerState(hand: Seq[Card])
  }

  /**
   * Player has answered to [[AskingForReservations]], but other players have not answered yet.
   */
  case class WaitingForReservations(
    playerState: Option[WaitingForReservations.PlayerState]
  ) extends GameState {
    override type PlayerState = WaitingForReservations.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
    def ownReservation: Option[Reservation] = playerOpt(_.ownReservation)
  }

  object WaitingForReservations {

    /**
     * Player has answered to [[AskingForReservations]], but other players have not answered yet.
     *
     * @param hand           The cards that have been dealt to the player.
     * @param ownReservation The reservations the player has called (if any).
     */
    case class PlayerState(hand: Seq[Card], ownReservation: Option[Reservation])
  }

  case class PovertyOnOffer(
    sizeOfPoverty: Int,
    playerOffering: PlayerPosition,
    playerState: Option[PovertyOnOffer.PlayerState]
  ) extends GameState {
    override type PlayerState = PovertyOnOffer.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
    def playerIsBeingAsked: Boolean = playerState.exists(_.playerIsBeingAsked)
  }

  object PovertyOnOffer {
    case class PlayerState(hand: Seq[Card], playerIsBeingAsked: Boolean)
  }

  case class PovertyRefused(playerState: Option[PovertyRefused.PlayerState.type])
    extends GameState {
    override type PlayerState = PovertyRefused.PlayerState.type
  }

  object PovertyRefused {
    case object PlayerState
  }

  case class PovertyExchange(
    sizeOfPoverty: Int,
    playerOffering: PlayerPosition,
    playerAccepting: PlayerPosition,
    playerState: Option[PovertyExchange.PlayerState]
  ) extends GameState {
    override type PlayerState = PovertyExchange.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
    def role: PovertyExchange.Role =
      playerState.fold[PovertyExchange.Role](PovertyExchange.NotInvolved)(_.role)
  }

  object PovertyExchange {

    case class PlayerState(hand: Seq[Card], role: PovertyExchange.Role)

    sealed trait Role

    case object Poor extends Role

    case object Accepting extends Role

    case object NotInvolved extends Role

  }

  /**
   * Game is in progress.
   *
   * @param currentTrick      The trick that is being played.
   * @param bids              Any bids that were called so far.
   * @param reservation       The active reservation if any.
   * @param trickCounts       How many tricks each player has won so far
   * @param trickWinner       The winner of the current trick.
   */
  case class Playing(
    currentTrick: Trick,
    bids: Map[PlayerPosition, NameableBid],
    reservation: Option[(PlayerPosition, LastingReservation)],
    trickCounts: Map[PlayerPosition, Int],
    playerState: Option[Playing.PlayerState],
    trickWinner: Option[PlayerPosition]
  ) extends GameState {
    override type PlayerState = Playing.PlayerState
    def hand: Seq[Card] = playerSeq(_.hand)
    def possibleBid: Option[Bid] = playerOpt(_.possibleBid)
    def canPlay: Set[Card] = playerSet(_.canPlay)
  }

  object Playing {

    /**
     * @param hand              The cards of the player.
     * @param isElders          Whether the player is part of the "elder" team.
     * @param possibleBid       The bid the player could call next.
     * @param canPlay           The cards that the player if allowed to play.
     * @param needsAck          True if the player needs to acknowledge the trick result
     */
    case class PlayerState(
      hand: Seq[Card],
      isElders: Boolean,
      possibleBid: Option[Bid],
      canPlay: Set[Card],
      needsAck: Boolean
    )
  }

  /**
   * End of the round - results of the round are presented to each player.
   * @param scores The results of the round.
   */
  case class RoundResults(
    scores: Scores,
    playerState: Option[RoundResults.PlayerState.type]
  ) extends GameState {
    override type PlayerState = RoundResults.PlayerState.type
  }

  object RoundResults {
    case object PlayerState
  }
}

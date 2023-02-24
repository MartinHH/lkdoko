package io.github.mahh.doko.client

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.AskingForReservations
import io.github.mahh.doko.shared.game.GameState.Playing
import io.github.mahh.doko.shared.game.GameState.PovertyExchange
import io.github.mahh.doko.shared.game.GameState.PovertyOnOffer
import io.github.mahh.doko.shared.game.GameState.PovertyRefused
import io.github.mahh.doko.shared.game.GameState.ReservationResult
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.game.GameState.WaitingForReservations
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition

case class CardConfig(card: Option[Card], action: Option[PlayerAction[GameState]]):
  def imageSrc: String = SvgPaths.getSvgUri(card)

object CardConfig:
  def apply(card: Card, action: Option[PlayerAction[GameState]] = None): CardConfig =
    CardConfig(Some(card), action)

  val empty: CardConfig = CardConfig(None, None)

  def handForState(state: GameState): Seq[CardConfig] = state match {
    case _: PovertyRefused | _: RoundResults =>
      Seq.empty
    case a: AskingForReservations =>
      a.hand.map(CardConfig(_))
    case w: WaitingForReservations =>
      w.hand.map(CardConfig(_))
    case r: ReservationResult =>
      r.hand.map(CardConfig(_))
    case p: PovertyOnOffer =>
      p.hand.map(CardConfig(_))
    case p: PovertyExchange =>
      val selected: Seq[Card] = p.playerState.fold(Seq.empty)(_.selected)
      val isAccepting = p.role == PovertyExchange.Accepting
      val action: Card => Option[PlayerAction[PovertyExchange]] = card =>
        if (isAccepting && selected.size < p.sizeOfPoverty) {
          Some(PlayerAction.PovertySelect(card))
        } else {
          None
        }
      p.hand.map(c => CardConfig(c, action(c)))
    case p: Playing =>
      p.hand.map { c =>
        CardConfig(c, if (p.canPlay(c)) Some(PlayerAction.PlayCard(c)) else None)
      }
  }

  def trickForState(state: GameState): Map[PlayerPosition, CardConfig] = state match {
    case _: PovertyRefused | _: RoundResults | _: AskingForReservations |
        _: WaitingForReservations | _: ReservationResult | _: PovertyOnOffer =>
      Map.empty
    case p: PovertyExchange =>
      // note: this is only valid as long as the "trick area" is also used for displaying the
      // selection of "cards to return" in poverty exchange
      val selected: Seq[Card] = p.playerState.fold(Seq.empty)(_.selected)
      val cardMap = PlayerPosition.All.zip(selected).toMap
      cardMap.map { case (p, c) => p -> CardConfig(c, Some(PlayerAction.PovertyDeselect(c))) }
    case p: Playing =>
      val action =
        if (p.playerState.exists(_.needsAck)) Some(PlayerAction.AcknowledgeTrickResult) else None
      p.currentTrick.cards.map { case (p, c) => p -> CardConfig(c, action) }
  }

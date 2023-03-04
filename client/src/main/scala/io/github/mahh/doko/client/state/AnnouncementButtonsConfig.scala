package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.game.GameState

/**
 * Configuration state for buttons that are shown in the "announcement area".
 */
enum AnnouncementButtonsConfig:
  case NoButtons
  case PovertyOffered
  case PovertyExchange(canReturn: Boolean)

object AnnouncementButtonsConfig:
  def forState(state: GameState): AnnouncementButtonsConfig = state match {
    case p: GameState.PovertyExchange if p.isAccepting =>
      PovertyExchange(p.canReturn)
    case p: GameState.PovertyOnOffer if p.playerIsBeingAsked =>
      PovertyOffered
    case _ =>
      NoButtons
  }

  def forState(state: ClientGameState): AnnouncementButtonsConfig = state match {
    case ClientGameState.GameInProgress(gs) =>
      forState(gs)
    case _ =>
      NoButtons
  }

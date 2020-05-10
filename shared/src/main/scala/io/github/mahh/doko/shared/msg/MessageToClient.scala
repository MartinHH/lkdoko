package io.github.mahh.doko.shared.msg

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores

/**
 * Messages that are sent from server to client (via websocket).
 */
sealed trait MessageToClient

object MessageToClient {

  /** The current nicknames of the players. */
  case class PlayersMessage(players: Map[PlayerPosition, String]) extends MessageToClient

  /** Signals that one or more players are "away from table" (e.g. having connection troubles). */
  case class PlayersOnPauseMessage(missingPlayers: Set[PlayerPosition]) extends MessageToClient

  /** The current state of the game. */
  case class GameStateMessage(gameState: GameState) extends MessageToClient

  /** The total scores. */
  case class TotalScoresMessage(totalScore: TotalScores) extends MessageToClient

  /** Response that is sent if a player tries to join a "table" that already has four players. */
  case object TableIsFull extends MessageToClient
}

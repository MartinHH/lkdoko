package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.game.GameState

/**
 * Combines (user-)relevant aspects of connection state with game-state.
 */
enum ClientGameState:

  case Connecting
  case Joining
  case GameInProgress(state: GameState)
  case Error(msg: String)

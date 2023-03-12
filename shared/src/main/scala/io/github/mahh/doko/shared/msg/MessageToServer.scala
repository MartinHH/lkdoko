package io.github.mahh.doko.shared.msg

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction

/**
 * Messages that are sent from client to server (via websocket).
 */
sealed trait MessageToServer

object MessageToServer {

  case class PlayerActionMessage(action: PlayerAction[GameState]) extends MessageToServer

  case class SetUserName(name: String) extends MessageToServer

}

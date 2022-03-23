package io.github.mahh.doko.shared.msg

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.json.Json
import io.github.mahh.doko.shared.player.PlayerAction

/**
 * Messages that are sent from client to server (via websocket).
 */
sealed trait MessageToServer

object MessageToServer {

  case class PlayerActionMessage(action: PlayerAction[GameState]) extends MessageToServer

  case class SetUserName(name: String) extends MessageToServer

  import io.circe.generic.auto._
  import io.circe.generic.semiauto._

  private[this] val encoder: Json.Encoder[MessageToServer] = deriveEncoder

  private[this] val decoder: Json.Decoder[MessageToServer] = deriveDecoder

  implicit def messageToServerEncoder: Json.Encoder[MessageToServer] = encoder

  implicit def messageToServerDecoder: Json.Decoder[MessageToServer] = decoder
}

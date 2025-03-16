package io.github.mahh.doko.shared.json

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.TotalScores

object Protocol {
  import Json.*

  private given KeyDecoder[PlayerPosition] = {
    val lookup = PlayerPosition.All.map(pos => pos.toString -> pos).toMap
    KeyDecoder.instance(lookup.get)
  }

  private given KeyEncoder[PlayerPosition] =
    KeyEncoder[String].contramap(_.toString)

  given Encoder[MessageToClient] = Encoder.derived

  given Decoder[MessageToClient] = Decoder.derived

  given Encoder[MessageToServer] = Encoder.derived

  given Decoder[MessageToServer] = Decoder.derived
}

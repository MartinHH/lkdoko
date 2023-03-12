package io.github.mahh.doko.shared.json

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.TotalScores

object Protocol {
  import Json.*

  // circe derivation does a lot of inlining - to avoid setting -Xmax-inlines even higher (it's at
  // 80 already), here we define various private intermediate instances:

  private given Encoder[NameableBid] = Encoder.derived

  private given Decoder[NameableBid] = Decoder.derived

  private given Encoder[Card] = Encoder.derived

  private given Decoder[Card] = Decoder.derived

  private given Encoder[PlayerPosition] = Encoder.derived

  private given Decoder[PlayerPosition] = Decoder.derived

  private given KeyDecoder[PlayerPosition] = {
    val lookup = PlayerPosition.All.map(pos => pos.toString -> pos).toMap
    KeyDecoder.instance(lookup.get)
  }

  private given KeyEncoder[PlayerPosition] =
    KeyEncoder[String].contramap(_.toString)

  private given Encoder[Reservation] = Encoder.derived

  private given Decoder[Reservation] = Decoder.derived

  private given Encoder[Score] = Encoder.derived

  private given Decoder[Score] = Decoder.derived

  private given Encoder[Scores] = Encoder.derived

  private given Decoder[Scores] = Decoder.derived

  private given Encoder[TotalScores] = Encoder.derived

  private given Decoder[TotalScores] = Decoder.derived

  given Encoder[MessageToClient] = Encoder.derived

  given Decoder[MessageToClient] = Decoder.derived

  given Encoder[MessageToServer] = Encoder.derived

  given Decoder[MessageToServer] = Decoder.derived
}

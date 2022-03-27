package io.github.mahh.doko.shared.player

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.github.mahh.doko.shared.json.Json._

/**
 * Position of a player at the table.
 */
sealed trait PlayerPosition derives Encoder, Decoder

object PlayerPosition {

  case object Player1 extends PlayerPosition

  case object Player2 extends PlayerPosition

  case object Player3 extends PlayerPosition

  case object Player4 extends PlayerPosition

  val All: List[PlayerPosition] = List(Player1, Player2, Player3, Player4)

  val AllAsSet: Set[PlayerPosition] = All.toSet

  val TotalNumberOfPlayers: Int = All.size

  val indexOf: PlayerPosition => Int = All.zipWithIndex.toMap

  val next: PlayerPosition => PlayerPosition =
    Map[PlayerPosition, PlayerPosition](
      Player1 -> Player2,
      Player2 -> Player3,
      Player3 -> Player4,
      Player4 -> Player1
    )

  def playingOrder(start: PlayerPosition): Seq[PlayerPosition] = LazyList.iterate(start)(next)

  def trickOrder(start: PlayerPosition): Seq[PlayerPosition] =
    playingOrder(start).take(PlayerPosition.TotalNumberOfPlayers)

  implicit val keyDecoder: KeyDecoder[PlayerPosition] = {
    val lookup = All.map(pos => pos.toString -> pos).toMap
    KeyDecoder.instance(lookup.get)
  }

  implicit val keyEncoder: KeyEncoder[PlayerPosition] =
    KeyEncoder[String].contramap(_.toString)
}

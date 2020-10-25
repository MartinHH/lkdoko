package io.github.mahh.doko.shared.table

import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.utils.ToStringUtil

/**
 * Holds values for all players and makes them available by position via a map-like api.
 *
 * This allows to ensure (at compile-time) that values for all players are present (as opposed
 * to a regular Map where we'd need to ensure completeness at runtime).
 */
case class TableMap[V](
  player1Val: V,
  player2Val: V,
  player3Val: V,
  player4Val: V
) extends (PlayerPosition => V) {

  def values: Seq[V] = List(player1Val, player2Val, player3Val, player4Val)

  def map[U](f: V => U): TableMap[U] = mapWithPos((_, v) => f(v))

  def mapWithPos[U](f: (PlayerPosition, V) => U): TableMap[U] = TableMap(
    f(PlayerPosition.Player1, player1Val),
    f(PlayerPosition.Player2, player2Val),
    f(PlayerPosition.Player3, player3Val),
    f(PlayerPosition.Player4, player4Val)
  )

  def +(kv: (PlayerPosition, V)): TableMap[V] = kv match {
    case (PlayerPosition.Player1, v) => copy(player1Val = v)
    case (PlayerPosition.Player2, v) => copy(player2Val = v)
    case (PlayerPosition.Player3, v) => copy(player3Val = v)
    case (PlayerPosition.Player4, v) => copy(player4Val = v)
  }

  override def apply(pos: PlayerPosition): V = pos match {
    case PlayerPosition.Player1 => player1Val
    case PlayerPosition.Player2 => player2Val
    case PlayerPosition.Player3 => player3Val
    case PlayerPosition.Player4 => player4Val
  }

  def toMap: Map[PlayerPosition, V] = Map(
    PlayerPosition.Player1 -> player1Val,
    PlayerPosition.Player2 -> player2Val,
    PlayerPosition.Player3 -> player3Val,
    PlayerPosition.Player4 -> player4Val
  )

  def collect[U](pf: PartialFunction[(PlayerPosition, V), U]): Iterable[U] = {
    toMap.collect(pf)
  }

  def keySet: Set[PlayerPosition] = PlayerPosition.AllAsSet

  override def toString: String = ToStringUtil.productToString(this)

}

object TableMap {


  def fromMap[V](map: Map[PlayerPosition, V]): Option[TableMap[V]] = {
    if (map.keySet == PlayerPosition.AllAsSet) {
      Some(TableMap(
        map(PlayerPosition.Player1),
        map(PlayerPosition.Player2),
        map(PlayerPosition.Player3),
        map(PlayerPosition.Player4)
      ))
    } else {
      None
    }
  }

  def fill[V](value: V): TableMap[V] = TableMap(value, value, value, value)

}

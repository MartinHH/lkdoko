package io.github.mahh.doko.client.laminar

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.player.PlayerPosition

object Tables:

  case class PlayerRowConfig[A](
    data: Signal[A],
    toContent: (PlayerPosition, A) => String,
    cellClass: String
  ):
    def cellContent(pos: PlayerPosition): Signal[String] = data.map(toContent(pos, _))

  object PlayerRowConfig:
    def apply[A](data: Signal[A], cellClass: String = "player-cell")(
      toContent: (PlayerPosition, A) => String
    ): PlayerRowConfig[A] =
      PlayerRowConfig(data, toContent, cellClass)

    def fromMap[V](data: Signal[Map[PlayerPosition, V]], cellClass: String = "player-cell")(
      toString: V => String
    )(
      default: PlayerPosition => String
    ): PlayerRowConfig[Map[PlayerPosition, V]] =
      PlayerRowConfig[Map[PlayerPosition, V]](data, cellClass) { (pos, map) =>
        map.get(pos).fold(default(pos))(toString)
      }

  private def tableCell(
    content: Signal[String],
    cellClass: String
  ): Div =
    div(
      cls := cellClass,
      child <-- content.map(textToNode)
    )

  private def playerTableRow[A](
    config: PlayerRowConfig[A]
  ): Div =
    div(
      cls := "player-row",
      children <-- Signal.fromValue(
        PlayerPosition.All.map(p => tableCell(config.cellContent(p), config.cellClass))
      )
    )

  private def playerTable(
    content: PlayerRowConfig[_]*
  ): Div =
    div(
      cls := "player-table",
      children <-- Signal.fromValue(content.map(playerTableRow))
    )

  def gameTable(
    playerNames: Signal[Map[PlayerPosition, String]],
    marker: Signal[Option[(PlayerPosition, String)]],
    bids: Signal[Map[PlayerPosition, NameableBid]],
    trickCounts: Signal[Map[PlayerPosition, Int]]
  ): Div = {
    playerTable(
      PlayerRowConfig.fromMap(playerNames)(identity)(_.toString),
      PlayerRowConfig(marker) {
        case (pos, Some(p, m)) if p == pos => m
        case _                             => ""
      },
      PlayerRowConfig.fromMap(bids, "bids-cell")(BidStrings.default.summaryString)(_ => ""),
      PlayerRowConfig.fromMap(trickCounts)(_.toString)(_ => 0.toString)
    )
  }

  def scoresTable(
    playerNames: Signal[Map[PlayerPosition, String]],
    scores: Signal[Map[PlayerPosition, Int]]
  ): Div = {
    playerTable(
      PlayerRowConfig.fromMap(playerNames)(identity)(_.toString),
      PlayerRowConfig.fromMap(scores)(_.toString)(_ => 0.toString)
    )
  }
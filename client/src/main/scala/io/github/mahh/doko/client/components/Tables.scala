package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.PlayerMarker
import io.github.mahh.doko.client.strings.BidStrings
import io.github.mahh.doko.client.strings.ScoreStrings
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.Score
import io.github.mahh.doko.shared.score.Scores
import io.github.mahh.doko.shared.score.Scores.TeamScore
import io.github.mahh.doko.shared.table.TableMap

object Tables:

  case class PlayerRowConfig[A](
    data: Signal[A],
    toContent: (PlayerPosition, A) => String,
    title: String,
    cellClass: String
  ):
    def cellContent(pos: PlayerPosition): Signal[String] = data.map(toContent(pos, _))

  object PlayerRowConfig:
    def apply[A](data: Signal[A], title: String = "", cellClass: String = "player-cell")(
      toContent: (PlayerPosition, A) => String
    ): PlayerRowConfig[A] =
      PlayerRowConfig(data, toContent, title, cellClass)

    def fromMap[V](
      data: Signal[Map[PlayerPosition, V]],
      title: String = "",
      cellClass: String = "player-cell"
    )(
      toString: V => String
    )(
      default: PlayerPosition => String
    ): PlayerRowConfig[Map[PlayerPosition, V]] =
      PlayerRowConfig[Map[PlayerPosition, V]](data, title, cellClass) { (pos, map) =>
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
        tableCell(Signal.fromValue(config.title), "row-header-cell") +:
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
    playerNames: Signal[TableMap[String]],
    marker: Signal[Option[PlayerMarker]],
    bids: Signal[Map[PlayerPosition, NameableBid]],
    trickCounts: Signal[Map[PlayerPosition, Int]],
    scores: Signal[Map[PlayerPosition, Int]]
  ): Div = {
    // TODO: translation instead of hardcoded titles
    playerTable(
      PlayerRowConfig(playerNames)((pos, map) => map(pos)),
      PlayerRowConfig(marker) {
        case (pos, Some(PlayerMarker.Next(p))) if p == pos        => "^"
        case (pos, Some(PlayerMarker.TrickWinner(p))) if p == pos => "*"
        case _                                                    => ""
      },
      PlayerRowConfig.fromMap(bids, title = "Ansagen", cellClass = "bids-cell")(
        BidStrings.default.summaryString
      )(_ => ""),
      PlayerRowConfig.fromMap(trickCounts, title = "Stiche")(_.toString)(_ => 0.toString),
      PlayerRowConfig.fromMap(scores, title = "Punkte")(_.toString)(_ => 0.toString)
    )
  }

  private def scoreString(s: Score): String = s"${ScoreStrings.default.toString(s)}: ${s.value}"

  def roundResultsTable(
    results: Signal[Option[RoundResults]],
    playerNames: Signal[TableMap[String]]
  ): Div =
    def cell(content: Signal[List[Node]]): Div = div(
      children <-- content,
      cls := "results-cell"
    )

    def rows(results: Signal[RoundResults]): List[Div] =
      val scores = results.map(_.scores)
      def row(title: String)(content: Signal[TeamScore] => Signal[List[Node]]): Div =
        val teams = List[Scores => TeamScore](_.eldersScore, _.othersScore)
        val teamCells = teams.map { f =>
          cell(content(scores.map(f)))
        }
        div(
          cls := "results-row",
          children <-- Signal.fromValue(
            tableCell(Signal.fromValue(title), "row-header-cell") +: teamCells
          )
        )

      def teamNames(teamScore: Signal[TeamScore]): Signal[List[Node]] =
        val names: Signal[List[String]] =
          teamScore
            .map(_.team.toList)
            .flatMap { t =>
              playerNames.map(pn => t.map(p => pn(p)))
            }(SwitchSignalStrategy)
        stringsWithLineBreaks(names)

      // TODO: translation instead of hardcoded titles
      List(
        row("Team")(teamNames),
        row("Kartenwert")(_.map(s => List(intToNode(s.tricksValue)))),
        row("Punkte")(teamScore => stringsWithLineBreaks(teamScore.map(_.scores.map(scoreString)))),
        row("Summe")(_.map(s => List(intToNode(s.total))))
      )
    div(
      cls := "results-table",
      children <-- results.split(_ => ())((_, _, r) => rows(r)).map(_.getOrElse(List.empty))
    )

package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.CardConfig
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition

object Cards {

  def card(
    config: Signal[CardConfig],
    actionSink: Observer[PlayerAction[GameState]],
    clss: String
  ): Image =
    img(
      src <-- config.map(_.imageSrc),
      observeClicksWithActions(config.map(_.action), actionSink),
      cls := clss
    )

  def trick(
    trick: Signal[Map[PlayerPosition, CardConfig]],
    actionSink: Observer[PlayerAction[GameState]]
  ): Div =
    val allFour: Signal[List[CardConfig]] =
      trick.map(tm => PlayerPosition.All.map(pos => tm.getOrElse(pos, CardConfig.empty)))
    val placeHolder = card(Signal.fromValue(CardConfig.empty), actionSink, "trick-card")
    val cardStream: Signal[List[Image]] =
      allFour
        .map(_.zipWithIndex)
        .split { case (_, i) => i } { (_, _, indexedCard) =>
          card(indexedCard.map { case (c, _) => c }, actionSink, "trick-card")
        }
        .map(placeHolder +: _)
    div(
      children <-- cardStream
    )

  def hand(
    hand: Signal[Seq[CardConfig]],
    actionSink: Observer[PlayerAction[GameState]]
  ): Div =
    val cards: Signal[Seq[Node]] =
      hand.map(_.zipWithIndex).split { case (_, i) => i } { (_, _, indexedCard) =>
        div(
          card(indexedCard.map { case (c, _) => c }, actionSink, "hand-card"),
          cls := "hand-card-container"
        )
      }
    div(
      children <-- cards
    )
}

package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.CardConfig
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition

object Cards {

  def card(
    config: Signal[CardConfig],
    actionSink: PlayerAction[GameState] => Unit,
    clss: String
  ): Image =
    val clickEventStream = new EventBus[org.scalajs.dom.MouseEvent]
    val clickActions: Observable[PlayerAction[GameState]] =
      config
        .flatMap(c => clickEventStream.toObservable.map(_ => c.action))(SwitchStreamStrategy)
        .collect { case Some(action) => action }
    img(
      src <-- config.map(_.imageSrc),
      onClick --> clickEventStream,
      clickActions --> actionSink,
      cls := clss
    )

  def trick(
    trick: Signal[Map[PlayerPosition, CardConfig]],
    actionSink: PlayerAction[GameState] => Unit
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
    actionSink: PlayerAction[GameState] => Unit
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

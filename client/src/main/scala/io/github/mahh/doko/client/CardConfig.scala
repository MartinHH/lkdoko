package io.github.mahh.doko.client

import io.github.mahh.doko.shared.deck.Card

case class CardConfig(card: Option[Card], callback: () => Unit = NoopCallback):
  def imageSrc: String = SvgPaths.getSvgUri(card)

object CardConfig:
  def apply(card: Card, callback: () => Unit): CardConfig =
    CardConfig(Some(card), callback)
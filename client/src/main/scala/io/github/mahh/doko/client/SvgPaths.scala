package io.github.mahh.doko.client

import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Rank
import io.github.mahh.doko.shared.deck.Suit
import org.scalajs.dom

object SvgPaths {


  def getSvgUri(card: Card): String = {
    val suit = card.suit match {
      case Suit.♣ => 'c'
      case Suit.♠ => 's'
      case Suit.♥ => 'h'
      case Suit.♦ => 'd'
    }
    val rank = card.rank match {
      case Rank.A => 'a'
      case Rank.Ten => 't'
      case Rank.K => 'k'
      case Rank.Q => 'q'
      case Rank.J => 'j'
      case Rank.Nine => '9'
    }

    val loc = dom.document.location
    s"${loc.protocol}//${loc.host}/cards?card=$suit$rank"
  }
}

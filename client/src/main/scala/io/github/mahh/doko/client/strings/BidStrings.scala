package io.github.mahh.doko.client.strings

import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.bids.Bid.NameableBid

// TODO: replace language mechanism by whatever the js/scala.js way to this is

trait BidStrings {

  def toString(bid: NameableBid): String

  def summaryString(bid: NameableBid): String = bid.bid match {
    case Bid.Win =>
      toString(bid)
    case _ =>
      s"${toString(bid.copy(bid = Bid.Win))}, ${toString(bid)}"
  }
}

object BidStrings {

  def default: BidStrings = German

  object German extends BidStrings {
    override def toString(bid: NameableBid): String = {
      import Bid._
      import BidExtension._
      bid.bid match {
        case Win if bid.isElders => "Re"
        case Win                 => "Kontra"
        case No90                => "Keine 90"
        case No60                => "Keine 60"
        case No30                => "Keine 30"
        case Blank               => "Blank"
      }
    }
  }

}

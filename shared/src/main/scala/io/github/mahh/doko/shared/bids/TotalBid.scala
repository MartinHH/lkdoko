package io.github.mahh.doko.shared.bids

import io.github.mahh.doko.shared.bids.TotalBid.BidExtension
import io.github.mahh.doko.shared.bids.TotalBid.SimpleBid

case class TotalBid(simpleBid: SimpleBid, extension: Option[BidExtension])

object TotalBid {
  sealed trait Bid

  sealed trait SimpleBid extends Bid

  object SimpleBid {
    case object Re extends SimpleBid
    case object Contra extends SimpleBid
  }

  sealed abstract class BidExtension(val limit: Int) extends Bid

  object BidExtension {
    case object No90 extends BidExtension(89)
    case object No60 extends BidExtension(59)
    case object No30 extends BidExtension(29)
    case object Blank extends BidExtension(0)

    val All: List[BidExtension] = List(No90, No60, No30, Blank)
  }
}


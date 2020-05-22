package io.github.mahh.doko.shared.bids

import io.github.mahh.doko.shared.bids.WinningBid.BidExtension
import io.github.mahh.doko.shared.utils.OrderingFromSeq

// TODO: most certainly, we only need bid, not the WinningBid wrapper

case class WinningBid(extension: Option[BidExtension])

object WinningBid {

  def apply(bid: Bid): WinningBid = bid match {
    case Win => WinningBid(None)
    case e: BidExtension => WinningBid(Some(e))
  }

  implicit val ordering: Ordering[WinningBid] = Ordering.by(_.extension)

  sealed trait Bid

  object Bid {
    val All: List[Bid] = Win :: BidExtension.All

    implicit val ordering: Ordering[Bid] = OrderingFromSeq(All)
  }

  case object Win extends Bid

  sealed abstract class BidExtension(val limit: Int) extends Bid

  object BidExtension {

    case object No90 extends BidExtension(89)

    case object No60 extends BidExtension(59)

    case object No30 extends BidExtension(29)

    case object Blank extends BidExtension(0)

    val All: List[BidExtension] = List(No90, No60, No30, Blank)

    implicit val ordering: Ordering[BidExtension] = OrderingFromSeq(All)
  }

}


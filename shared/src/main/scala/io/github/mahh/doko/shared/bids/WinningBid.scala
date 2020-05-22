package io.github.mahh.doko.shared.bids

import io.github.mahh.doko.shared.utils.OrderingFromSeq


object WinningBid {


  /**
   * Adds the "is elders" information so that the name of the bid ("re"/"kontra") can be derived.
   */
  case class NameableBid(isElders: Boolean, bid: Bid)

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


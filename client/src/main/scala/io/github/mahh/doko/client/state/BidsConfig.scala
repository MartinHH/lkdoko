package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.bids.Bid

case class BidsConfig(isElders: Boolean = false, possibleBid: Option[Bid] = None)

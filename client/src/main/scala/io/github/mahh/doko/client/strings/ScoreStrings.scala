package io.github.mahh.doko.client.strings

import io.github.mahh.doko.shared.bids.Bid.BidExtension
import io.github.mahh.doko.shared.score.Score

// TODO: replace language mechanism by whatever the js/scala.js way to this is

trait ScoreStrings {

  def toString(score: Score): String

}

object ScoreStrings {

  def default: ScoreStrings = German

  object German extends ScoreStrings {
    override def toString(score: Score): String = {
      import Score._
      score match {
        case Won =>
          "Gewonnen"
        case AgainstTheElders =>
          "Gegen die Alten"
        case WinCalled =>
          "Gewonnen Angesagt"
        case PlayedBelow(x) =>
          bidExtensionString(x)
        case PlayedBelowCalled(x) =>
          s"${bidExtensionString(x)} angesagt"
        case PlayedBelowCalledByOpponent(x) =>
          s"${bidExtensionString(x)} vom Gegner angesagt"
        case FoxCaught =>
          "Fuchs gefangen"
        case Charly =>
          "Charly"
        case CharlyCaught =>
          "Charly gefangen"
        case DoKo =>
          "Doppelkopf"
      }
    }

    private def bidExtensionString(bidExtension: BidExtension): String = bidExtension match {
      case BidExtension.Blank => "Blank"
      case x => s"Keine ${x.limit + 1}"
    }
  }

}

package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.client.state.Signals
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.github.mahh.doko.shared.player.PlayerAction

/**
 * "Root" component that combines all other components.
 */
object App {

  def apply(
    signals: Signals,
    actionSink: PlayerAction[GameState] => Unit,
    commandSink: SetUserName => Unit
  ): Div =
    import signals.*
    div(
      StringComponents.nameInput(nameInputHidden, commandSink),
      Tables.gameTable(playerNames, playerMarker, bids, trickCounts, totalScores),
      Areas.announcement(announcementString, buttonsConfig, actionSink),
      Cards.trick(trick, actionSink),
      p(),
      Buttons.bidButtons(bidsConfig, actionSink),
      p(),
      Cards.hand(hand, actionSink),
      Buttons.reservationButtons(possibleReservations, actionSink),
      Tables.roundResultsTable(results, playerNames),
      Buttons.countdownAckButton(ackConfig, actionSink)
    )

}

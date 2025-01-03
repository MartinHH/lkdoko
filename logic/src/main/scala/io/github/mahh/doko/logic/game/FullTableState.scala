package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.TransitionParams
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores

/**
 * Wraps the central `FullGameState`, adding some additional states that are not essential to the game logic.
 *
 * @param playerNames    Nicknames of each player.
 * @param gameState      The current state of the game.
 * @param missingPlayers Players that are currently "not at the table" (e.g. that have temporary connection
 *                       problems).
 */
case class FullTableState(
  playerNames: Map[PlayerPosition, String],
  gameState: FullGameState,
  missingPlayers: Set[PlayerPosition]
) {

  private val handlePlayerAction: PartialFunction[TransitionParams, FullTableState] = {

    val blockIfPlayersIsMissing: PartialFunction[TransitionParams, TransitionParams] = {
      case x if missingPlayers.isEmpty =>
        // no players missing, all calls can pass
        x
      case x @ (_, _: PlayerAction.Acknowledgement[_]) =>
        // player is missing, only acknowledgements may pass
        x
    }

    blockIfPlayersIsMissing andThen gameState.handleAction andThen (gs => copy(gameState = gs))
  }

  def handleAction(
    playerPosition: PlayerPosition,
    action: PlayerAction[GameState]
  ): Option[FullTableState] = {
    handlePlayerAction.lift(playerPosition, action)
  }

  def withUpdatedUserName(playerPosition: PlayerPosition, name: String): FullTableState = {
    copy(playerNames = playerNames + (playerPosition -> name))
  }

  val playerStates: Map[PlayerPosition, GameState] = gameState.playerStates.toMap

  def playerPauses(pos: PlayerPosition): FullTableState = {
    copy(missingPlayers = missingPlayers + pos)
  }

  def playerRejoins(pos: PlayerPosition): FullTableState = {
    copy(missingPlayers = missingPlayers - pos)
  }

  def totalScores: TotalScores = gameState.totalScores

}

object FullTableState {
  def apply(implicit rules: Rules): FullTableState = {
    FullTableState(Map.empty, FullGameState.initial, Set.empty)
  }
}

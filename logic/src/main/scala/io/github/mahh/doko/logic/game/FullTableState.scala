package io.github.mahh.doko.logic.game

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
  playerNames: Map[PlayerPosition, String] = Map.empty,
  gameState: FullGameState = FullGameState.initial,
  missingPlayers: Set[PlayerPosition] = Set.empty
) {

  val handlePlayerAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), FullTableState] = {

    if (missingPlayers.nonEmpty) {
      PartialFunction.empty
    } else {
      gameState.handleAction.andThen(gs => copy(gameState = gs))
    }

  }


  def handleAction(playerPosition: PlayerPosition, action: PlayerAction[GameState]): Option[FullTableState] = {
    handlePlayerAction.lift(playerPosition, action)
  }

  def withUpdatedUserName(playerPosition: PlayerPosition, name: String): FullTableState = {
    copy(playerNames = playerNames + (playerPosition -> name))
  }

  def playerStates: Map[PlayerPosition, GameState] = gameState.playerStates

  def playerPauses(pos: PlayerPosition): FullTableState = {
    copy(missingPlayers = missingPlayers + pos)
  }

  def playerRejoins(pos: PlayerPosition): FullTableState = {
    copy(missingPlayers = missingPlayers - pos)
  }

  def totalScores: TotalScores = gameState.totalScores

}

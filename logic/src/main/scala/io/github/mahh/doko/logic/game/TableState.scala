package io.github.mahh.doko.logic.game

import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
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
case class TableState(
  playerNames: Map[PlayerPosition, String] = Map.empty,
  gameState: FullGameState = FullGameState.initial,
  missingPlayers: Set[PlayerPosition] = Set.empty
) {

  val handlePlayerAction: PartialFunction[(PlayerPosition, PlayerAction[GameState]), TableState] = {

    if (missingPlayers.nonEmpty) {
      // TODO: this will block Join messages during joining phase if one of the players that already joined
      //  "pauses" - those should not be blocked:
      PartialFunction.empty
    } else {
      gameState.handleAction.andThen(gs => copy(gameState = gs))
    }

  }


  def handleMessage(playerPosition: PlayerPosition, msg: MessageToServer): Option[TableState] = msg match {
    case PlayerActionMessage(action) =>
      handlePlayerAction.lift(playerPosition, action)
    case SetUserName(name) =>
      Some(copy(playerNames = playerNames + (playerPosition -> name)))
  }

  def playerStates: Map[PlayerPosition, GameState] = gameState.playerStates

  def playerPauses(pos: PlayerPosition): TableState = {
    copy(missingPlayers = missingPlayers + pos)
  }

  def playerRejoins(pos: PlayerPosition): TableState = {
    copy(missingPlayers = missingPlayers - pos)
  }

  def totalScores: TotalScores = gameState.totalScores

}

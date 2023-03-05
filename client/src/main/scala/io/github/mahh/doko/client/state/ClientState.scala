package io.github.mahh.doko.client.state

import io.github.mahh.doko.client.strings.AnnouncementStrings
import io.github.mahh.doko.shared.bids.Bid.NameableBid
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.GameState.Playing
import io.github.mahh.doko.shared.game.GameState.RoundResults
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.table.TableMap

case class ClientState(
  gameState: ClientGameState,
  knownPlayerNames: Map[PlayerPosition, String],
  totalScores: Map[PlayerPosition, Int]
):

  def update(msg: MessageToClient): ClientState = msg match {
    case MessageToClient.Joining =>
      copy(gameState = ClientGameState.Joining)
    case MessageToClient.PlayersMessage(players) =>
      copy(knownPlayerNames = players)
    case MessageToClient.PlayersOnPauseMessage(_) =>
      // TODO: notify user that she needs to wait until all players are back
      this
    case MessageToClient.GameStateMessage(gameState) =>
      copy(gameState = ClientGameState.GameInProgress(gameState))
    case MessageToClient.TotalScoresMessage(totalScores) =>
      copy(totalScores = totalScores.sumPerPlayer)
    case MessageToClient.TableIsFull =>
      // covered via GaeState.playerState.isEmpty
      this
  }
  private def collectGameState[A](pf: PartialFunction[GameState, A]): Option[A] =
    gameState match {
      case ClientGameState.GameInProgress(pf(a)) =>
        Some(a)
      case _ =>
        None
    }

  private def collectGameStateOrElse[A](default: => A)(
    pf: PartialFunction[GameState, A]
  ): A = collectGameState(pf).getOrElse(default)

  val playerMarker: Option[PlayerMarker] = collectGameState { case p: Playing =>
    p.trickWinner.map(PlayerMarker.TrickWinner.apply) orElse
      p.currentTrick.currentPlayer.map(PlayerMarker.Next.apply)
  }.flatten

  val bids: Map[PlayerPosition, NameableBid] = collectGameStateOrElse(Map.empty) {
    case p: GameState.Playing => p.bids
  }

  val trickCounts: Map[PlayerPosition, Int] = collectGameStateOrElse(Map.empty) {
    case p: GameState.Playing => p.trickCounts
  }

  val possibleReservations: Option[Set[Reservation]] = collectGameState {
    case a: GameState.AskingForReservations => a.possibleReservations.toSet
  }

  val bidsConfig: BidsConfig =
    collectGameStateOrElse(BidsConfig()) { case p: GameState.Playing =>
      p.playerState.fold(BidsConfig()) { ps =>
        BidsConfig(ps.isElders, ps.possibleBid)
      }
    }

  val buttonsConfig: AnnouncementButtonsConfig =
    AnnouncementButtonsConfig.forState(gameState)

  val results: Option[RoundResults] = collectGameState { case rr: RoundResults => rr }

  val hand: Seq[CardConfig] = gameState match {
    case ClientGameState.GameInProgress(gs) =>
      CardConfig.handForState(gs)
    case _ =>
      Seq.empty
  }

  val trick: Map[PlayerPosition, CardConfig] = gameState match {
    case ClientGameState.GameInProgress(gs) =>
      CardConfig.trickForState(gs)
    case _ =>
      Map.empty
  }

  val ackConfig: Option[AckConfig] = collectGameStateOrElse(None) { case gs =>
    AckConfig.forState(gs)
  }

  val playerNames: TableMap[String] = TableMap.fromMapOrElse(knownPlayerNames, _.toString)

  val announcementString: String = AnnouncementStrings.forConnectionState(gameState, playerNames)

  val nameInputHidden: Boolean = gameState match {
    case ClientGameState.GameInProgress(s) =>
      s.playerState.isEmpty
    case ClientGameState.Connecting =>
      false
    case _ =>
      true
  }

object ClientState:
  val initial: ClientState = ClientState(ClientGameState.Connecting, Map.empty, Map.empty)

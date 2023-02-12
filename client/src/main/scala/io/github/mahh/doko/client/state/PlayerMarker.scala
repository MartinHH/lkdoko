package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.player.PlayerPosition

enum PlayerMarker:
  case TrickWinner(position: PlayerPosition)
  case Next(position: PlayerPosition)

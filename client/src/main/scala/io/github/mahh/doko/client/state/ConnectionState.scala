package io.github.mahh.doko.client.state

import io.github.mahh.doko.shared.player.PlayerPosition

enum ConnectionState {
  case Connected
  case Disconnected(willReconnect: Boolean)
  case Error(msg: String)
}

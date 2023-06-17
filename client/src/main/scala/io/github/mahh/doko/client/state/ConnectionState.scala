package io.github.mahh.doko.client.state

enum ConnectionState {
  case Connected
  case Disconnected(willReconnect: Boolean)
  case Error(msg: String)
}

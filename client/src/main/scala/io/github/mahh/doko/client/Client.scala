package io.github.mahh.doko.client

import com.raquo.laminar.api.L
import io.github.mahh.doko.client.components.App
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.laminext.websocket.*
import io.laminext.websocket.circe.*
import org.scalajs.dom

/**
 * The client's main.
 */
object Client {

  def main(args: Array[String]): Unit = {

    val ws: WebSocket[MessageToClient, MessageToServer] = WebSocket
      .path(s"/game${dom.document.location.search}")
      .json[MessageToClient, MessageToServer]
      .build()

    L.renderOnDomContentLoaded(
      dom.document.querySelector("#app"),
      App(ws)
    )
  }

}

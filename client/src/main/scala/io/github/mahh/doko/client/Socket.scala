package io.github.mahh.doko.client

import java.util.UUID

import io.circe
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import org.scalajs.dom
import org.scalajs.dom.CloseEvent
import org.scalajs.dom.Event
import org.scalajs.dom.MessageEvent
import org.scalajs.dom.raw.WebSocket

import scala.concurrent.duration.DurationLong
import scala.language.postfixOps
import scala.scalajs.js.timers._

/**
 * Wraps a websocket with some reconnect logic.
 *
 * Reconnect logic is strongly inspired by [[https://github.com/rleibman/scalajs-reconnecting-websocket]].
 */
class Socket {

  import Socket._

  private val id: UUID = UUID.randomUUID()
  private val url = websocketUri(id)

  private var socket = Option(open())
  private var listener: Option[Listener] = None
  private var keepConnected = true

  def write(msg: MessageToServer): Unit = socket.foreach { webSocket =>
    if (webSocket.readyState == WebSocket.OPEN) {
      webSocket.send(msg.asJson.noSpaces)
    }
  }

  def setListener(listener: Listener): Unit = {
    this.listener = Some(listener)
  }

  def close(): Unit = {
    keepConnected = false
    socket.foreach(_.close())
    socket = None
  }

  private def open(isReconnect: Boolean = false, attempt: Int = 0): WebSocket = {
    val webSocket = new WebSocket(url)


    var timedOut = false
    val timeout = setTimeout(timeoutInterval millis) {
      timedOut = true
      webSocket.close()
      timedOut = false
    }
    webSocket.onopen = {
      _: Event =>
        clearTimeout(timeout)
        listener.foreach(_.onOpen(isReconnect))
    }
    webSocket.onclose = {
      _: CloseEvent =>
        clearTimeout(timeout)
        socket = None
        if (keepConnected) {
          if (attempt > 0 && !timedOut) {
            listener.foreach(_.onError(s"Connection closed unexpectedly - reconnecting"))
          }

          import Math._
          // Per the RFC, wait a random time before retrying to connect
          // This algorithm for figuring out the reconnect time is called "binary exponential backoff"
          val apow = pow(1.5, attempt)
          val minlong = min(Long.MaxValue, apow)
          val rand = random() * minlong
          val interval = minReconnectInterval + (reconnectInterval * rand)
          val timeoutInterval = min(maxReconnectInterval.toDouble, interval)
          setTimeout(timeoutInterval) {
            socket = Some(open(isReconnect = true, attempt + 1))
          }
        }
    }
    webSocket.onmessage = {
      event: MessageEvent =>
        listener.foreach(_.onUpdate(decode[MessageToClient](event.data.toString)))
    }
    webSocket.onerror = { event =>
      listener.foreach(_.onError(event.toString))
    }
    webSocket
  }

}

object Socket {
  private def websocketUri(id: UUID): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}/game${dom.document.location.search}"
  }

  private val timeoutInterval: Long = 2000
  private val reconnectInterval: Int = 1000
  private val minReconnectInterval: Long = 200
  private val maxReconnectInterval: Long = 60000

  trait Listener {
    def onOpen(isReconnect: Boolean): Unit
    def onError(msg: String): Unit
    def onUpdate(update: Either[circe.Error, MessageToClient]): Unit
  }
}
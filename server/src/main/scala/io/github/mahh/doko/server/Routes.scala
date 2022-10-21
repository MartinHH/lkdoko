package io.github.mahh.doko.server

import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Flow
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.json.Json
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer

import scala.concurrent.Future
import scala.util.Failure

class Routes(logicFlowFactory: LogicFlowFactory)(implicit system: ActorSystem) extends Directives {

  import system.dispatcher

  def route: Route =
    get {
      pathSingleSlash {
        redirect(s"/table?id=${ParticipantId.random().toString}", StatusCodes.TemporaryRedirect)
      } ~
        path("table")(getFromResource("web/index.html")) ~
        path("client-fastopt.js")(getFromResource("client-fastopt.js")) ~
        path("client-fastopt.js.map")(getFromResource("client-fastopt.js.map")) ~
        path("game") {
          parameter("id") { id =>
            system.log.info(s"Connected: $id")
            handleWebSocketMessages(websocketFlow(ParticipantId.fromString(id)))
          }
        } ~
        path("cards") {
          parameter("card") { id =>
            getFromResource(s"svg/$id.svg")
          }
        }
    } ~
      getFromResourceDirectory("web")

  private def websocketFlow(id: ParticipantId): Flow[Message, Message, Any] =
    Flow[Message]
      .mapAsync(1) {
        case TextMessage.Strict(s)   => Future.successful(s)
        case TextMessage.Streamed(s) => s.runFold("")(_ + _)
        case _: BinaryMessage        => throw new Exception("Binary message cannot be handled")
      }
      .map(Json.decode[MessageToServer])
      // TODO: Handle parser errors (at least log them...)
      .collect { case Right(s) => s }
      .via(logicFlowFactory.flowForParticipant(id))
      .map(state => TextMessage(Json.encode(state)))
      .via(reportErrorsFlow)

  private def reportErrorsFlow[T]: Flow[T, T, Any] =
    Flow[T]
      .watchTermination()((_, f) =>
        f.onComplete {
          case Failure(cause) =>
            println(s"WS stream failed with $cause")
          case _ => // ignore regular completion
        }
      )
}

object Routes {
  private val OutgoingBufferSize = 16
}

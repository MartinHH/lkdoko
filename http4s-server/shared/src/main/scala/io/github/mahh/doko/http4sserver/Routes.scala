package io.github.mahh.doko.http4sserver

import cats.effect.Sync
import cats.effect.std.Queue
import cats.implicits.*
import fs2.Stream
import fs2.concurrent.Topic
import io.github.mahh.doko.http4sserver.resources.ResourceFiles
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.client.ClientId
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.json.Json
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.headers.Location
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.util.Try

object Routes:

  private object CardQueryParamMatcher extends QueryParamDecoderMatcher[String]("card")

  private object IdQueryParamMatcher extends QueryParamDecoderMatcher[String]("id")

  private def location[F[_]: Sync](name: String): F[Location] =
    Sync[F].pure(Location.parse(name)).rethrow

  def all[F[_]: Sync](
    queue: Queue[F, IncomingAction[ClientId]],
    topic: Topic[F, Map[ClientId, Seq[MessageToClient]]]
  )(
    webSocketBuilder: WebSocketBuilder[F]
  ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    def fromResource(
      name: String,
      req: Request[F]
    ): F[Response[F]] = {
      ResourceFiles.fromResource(name, Some(req)).getOrElseF(NotFound())
    }

    def websocket(
      participantId: ParticipantId,
      clientId: ClientId
    ): F[Response[F]] = {

      // outgoing messages (from `topic` to websocket):
      val toClient: Stream[F, WebSocketFrame.Text] =
        topic
          .subscribe(1000)
          .flatMap(msgMap => Stream.emits(msgMap.getOrElse(clientId, Vector.empty)))
          .map(msg => WebSocketFrame.Text(Json.encode(msg)))

      // incoming messages (from websocket to `queue`):
      def processInput(wsfStream: Stream[F, WebSocketFrame]): Stream[F, Unit] = {

        val clientJoinedStream: Stream[F, IncomingAction[ClientId]] =
          Stream.emit(IncomingAction.ClientJoined(clientId, participantId))

        val parsedWebSocketInput: Stream[F, IncomingAction[ClientId]] =
          wsfStream.collect { case WebSocketFrame.Text(text, _) =>
            val actionEither =
              Json.decode[MessageToServer](text).map { msg =>
                IncomingAction.IncomingMessage[ClientId](participantId, msg)
              }
            Stream.fromEither(actionEither)
          }.flatten

        (clientJoinedStream ++ parsedWebSocketInput)
          .handleErrorWith(_ => Stream.emit(IncomingAction.ClientLeft(clientId, participantId)))
          .evalMapChunk(queue.offer)
      }

      webSocketBuilder
        .withOnClose(queue.offer(IncomingAction.ClientLeft(clientId, participantId)))
        .build(toClient, processInput)
    }

    HttpRoutes.of[F] {
      case GET -> Root =>
        for {
          loc <- location[F](s"/table?id=${ParticipantId.random().toString}")
          resp <- TemporaryRedirect(loc)
        } yield resp
      case req @ GET -> Root / "table" =>
        fromResource("/web/index.html", req)
      case req @ GET -> Root / "client-fastopt.js" =>
        fromResource("/client-fastopt.js", req)
      case req @ GET -> Root / "client-fastopt.js.map" =>
        fromResource("/client-fastopt.js.map", req)
      case req @ GET -> Root / "cards" :? CardQueryParamMatcher(id) =>
        fromResource(s"/svg/$id.svg", req)
      case GET -> Root / "game" :? IdQueryParamMatcher(id) =>
        for {
          participantId <- Sync[F].fromTry(Try(ParticipantId.fromString(id)))
          clientId = ClientId.random()
          response <- websocket(participantId, clientId)
        } yield response

    }
  }

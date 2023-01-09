package io.github.mahh.doko.http4sserver

import cats.MonadThrow
import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.kernel.Spawn
import cats.effect.std.Queue
import cats.implicits.*
import com.comcast.ip4s.*
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.file.Files
import io.github.mahh.doko.http4sserver.utils.logging.LoggingCompanion
import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.client.ClientId
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware
import org.http4s.implicits.*
import org.http4s.server.websocket.WebSocketBuilder
import org.typelevel.log4cats.Logger

object ServerMain extends App {

  private def server[F[_]: Async: Spawn: Logger](
    queue: Queue[F, IncomingAction[ClientId]],
    topic: Topic[F, Map[ClientId, Seq[MessageToClient]]]
  ): F[Nothing] = {
    def websocketApp(wsb: WebSocketBuilder[F]): HttpApp[F] = {
      val httpApp = Routes.all[F](queue, topic)(wsb).orNotFound
      // middleware.Logger.httpApp(logHeaders = false, logBody = false)(httpApp)
      httpApp
    }

    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpWebSocketApp(websocketApp)
      .build
      .useForever
  }

  def run(args: List[String]): IO[ExitCode] =
    // TODO: configurable rules - make this configurable
    given rules: Rules = Rules(DeckRule.WithNines)
    given logger[F[_]: Sync]: Logger[F] = LoggingCompanion.getLogger[F]
    for {
      queue <- Queue.unbounded[IO, IncomingAction[ClientId]]
      topic <- Topic[IO, Map[ClientId, Seq[MessageToClient]]]
      tableFiber <- TableStream(queue, topic).compile.drain.start
      serverFiber <- server(queue, topic).start
      _ <- tableFiber.join
      _ <- serverFiber.join
    } yield ExitCode.Success
}

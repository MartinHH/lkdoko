package io.github.mahh.doko.http4sserver

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Queue
import com.comcast.ip4s.*
import fs2.concurrent.Topic
import fs2.io.net.Network
import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.client.ClientId
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.websocket.WebSocketBuilder
import org.typelevel.log4cats.LoggerFactory

object ServerMain extends IOApp {

  private def server[F[_]: Async: Network: LoggerFactory](
    queue: Queue[F, IncomingAction[ClientId]],
    topic: Topic[F, Map[ClientId, Seq[MessageToClient]]]
  ): F[Nothing] = {
    def websocketApp(wsb: WebSocketBuilder[F]): HttpApp[F] = {
      val httpApp = Routes.all[F](queue, topic)(wsb).orNotFound
      // org.http4s.server.middleware.Logger.httpApp(logHeaders = false, logBody = false)(httpApp)
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
    given LoggerFactory[IO] = utils.logging.ScribeLogger.factory
    for {
      queue <- Queue.unbounded[IO, IncomingAction[ClientId]]
      topic <- Topic[IO, Map[ClientId, Seq[MessageToClient]]]
      tableFiber <- TableStream(queue, topic).compile.drain.start
      serverFiber <- server(queue, topic).start
      _ <- tableFiber.join
      _ <- serverFiber.join
    } yield ExitCode.Success
}

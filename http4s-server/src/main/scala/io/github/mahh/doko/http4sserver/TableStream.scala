package io.github.mahh.doko.http4sserver

import cats.Applicative
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import io.github.mahh.doko.http4sserver.utils.logging.asLog4Cats
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.TableServerStateMachine
import io.github.mahh.doko.logic.table.TableServerStateMachine.TransitionResult
import io.github.mahh.doko.logic.table.client.ClientId
import io.github.mahh.doko.shared.msg.MessageToClient
import org.typelevel.log4cats.MessageLogger

object TableStream {

  /**
   * Creates a "state machine stream" that drains the `queue` and and publishes to `topic`.
   *
   * This stream represents the central server logic.
   */
  def apply[F[_]: Applicative: MessageLogger](
    queue: Queue[F, IncomingAction[ClientId]],
    topic: Topic[F, Map[ClientId, Seq[MessageToClient]]]
  )(using rules: Rules): Stream[F, Map[ClientId, Seq[MessageToClient]]] = {
    Stream
      .fromQueueUnterminated(queue)
      .scan(TransitionResult.initial[ClientId]) { (to, in) =>
        TableServerStateMachine.transition(to.state, in)
      }
      .evalMap(extractOutputAndLog)
      .through(topic.publish)
  }

  /** Logs the `result.logTasks` and returns the grouped `result.outgoingMessages`. */
  private def extractOutputAndLog[F[_]: Applicative: MessageLogger](
    result: TransitionResult[ClientId]
  ): F[Map[ClientId, Seq[MessageToClient]]] = {
    result.logTasks
      .map(_.asLog4Cats[F])
      .sequence
      .as(
        result.outgoingMessages.groupMap(_.clientRef)(_.message)
      )
  }
}

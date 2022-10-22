package io.github.mahh.doko.server.tableflow

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.BroadcastHub
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.MergeHub
import akka.stream.scaladsl.RunnableGraph
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.ClientMessageTask
import io.github.mahh.doko.logic.table.IncomingAction
import io.github.mahh.doko.logic.table.IncomingAction.ClientJoined
import io.github.mahh.doko.logic.table.IncomingAction.ClientLeft
import io.github.mahh.doko.logic.table.TableServerState
import io.github.mahh.doko.logic.table.TableServerStateMachine
import io.github.mahh.doko.logic.table.TableServerStateMachine.TransitionResult
import io.github.mahh.doko.logic.table.client.ClientId
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.server.LogicFlowFactory
import io.github.mahh.doko.server.tableflow
import io.github.mahh.doko.server.utils.logging.apply
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class FlowBasedLogicFlowFactory(using materializer: Materializer, rules: Rules)
  extends LogicFlowFactory {

  import FlowBasedLogicFlowFactory.*

  private given logger: Logger = LoggerFactory.getLogger(getClass)

  private val (sink, source) = tableFlow.run()

  override def flowForParticipant(
    id: ParticipantId
  ): Flow[MessageToServer, MessageToClient, NotUsed] = {
    Flow[MessageToServer]
      // flatMapPrefix(0) so that it is ensured that ClientId.random() gets called at runtime,
      // resulting in a different ID for each materialized instance
      .flatMapPrefix(0) { _ =>
        gameFlow(id, ClientId.random(), sink, source)
      }
  }
}

object FlowBasedLogicFlowFactory {

  // typedefs for the sake of sane line lengths:

  private type TableFlowSink = Sink[IncomingAction[ClientId], NotUsed]

  private type TableFlowSource = Source[Map[ClientId, Seq[MessageToClient]], NotUsed]

  // the core server logic: dynamic fan-in -> a state machine (using .scan) -> dynamic fan-out
  private def tableFlow(
    using rules: Rules,
    logger: Logger
  ): RunnableGraph[(TableFlowSink, TableFlowSource)] =
    MergeHub
      .source[IncomingAction[ClientId]]
      .scan(TransitionResult.initial[ClientId]) { (to, in) =>
        val result = TableServerStateMachine.transition(to.state, in)
        result.logTasks.foreach(logger.apply)
        result
      }
      .map { _.outgoingMessages.groupMap(_.clientRef)(_.message) }
      .toMat(BroadcastHub.sink)(Keep.both)

  // connects a single client to the fan-in and fan-out stages of a `tableFlow`
  private def gameFlow(
    participantId: ParticipantId,
    clientId: ClientId,
    logicIn: TableFlowSink,
    logicOut: TableFlowSource
  ): Flow[MessageToServer, MessageToClient, NotUsed] = {

    val sink = Flow[MessageToServer]
      .map[IncomingAction[ClientId]](IncomingAction.IncomingMessage(participantId, _))
      .prepend(Source.single(IncomingAction.ClientJoined(clientId, participantId)))
      .concat(Source.single(IncomingAction.ClientLeft(clientId, participantId)))
      .recover { case _ => IncomingAction.ClientLeft(clientId, participantId) }
      .to(logicIn)

    val source =
      logicOut
        .mapConcat(_.getOrElse(clientId, Vector.empty))

    Flow.fromSinkAndSourceCoupled(sink, source)
  }
}

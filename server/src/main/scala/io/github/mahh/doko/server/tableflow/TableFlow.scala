package io.github.mahh.doko.server.tableflow

import akka.NotUsed
import akka.event.Logging
import akka.event.slf4j.Logger
import akka.stream.Attributes
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
import io.github.mahh.doko.logic.table.TableServerState
import io.github.mahh.doko.logic.table.TableServerState.TransitionOutput
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.server.LogicFlowFactory
import io.github.mahh.doko.server.tableflow
import io.github.mahh.doko.server.tableflow.IncomingAction.ClientJoined
import io.github.mahh.doko.server.tableflow.IncomingAction.ClientLeft
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import io.github.mahh.doko.shared.msg.MessageToServer.PlayerActionMessage
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName

/** State transition logic. */
private def applyIncoming(
  state: TableServerState[ClientId],
  in: IncomingAction
): TransitionOutput[ClientId] =
  import IncomingAction.*
  in match
    case cj: ClientJoined =>
      state.applyClientJoined(cj.participantId, cj.clientId)
    case IncomingMessage(participantId, SetUserName(name)) =>
      state.applyUserNameChange(participantId, name).getOrElse(state -> Vector.empty)
    case IncomingMessage(participantId, PlayerActionMessage(action)) =>
      state.applyPlayerAction(participantId, action).getOrElse(state -> Vector.empty)
    case cl: ClientLeft =>
      state.applyClientLeft(cl.participantId, cl.clientId).getOrElse(state -> Vector.empty)

class FlowBasedLogicFlowFactory(using materializer: Materializer, rules: Rules)
  extends LogicFlowFactory {

  import FlowBasedLogicFlowFactory.*

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

  private type TableFlowSink = Sink[IncomingAction, NotUsed]

  private type TableFlowSource = Source[Map[ClientId, Vector[MessageToClient]], NotUsed]

  // the core server logic: dynamic fan-in -> a state machine (using .scan) -> dynamic fan-out
  private def tableFlow(
    using rules: Rules
  ): RunnableGraph[(TableFlowSink, TableFlowSource)] =
    MergeHub
      .source[IncomingAction]
      .scan(TransitionOutput.initial[ClientId]) { case ((state, _), in) =>
        applyIncoming(state, in)
      }
      .map { case (_, out) => out.groupMap(_.clientRef)(_.message) }
      .toMat(BroadcastHub.sink)(Keep.both)

  // connects a single client to the fan-in and fan-out stages of a `tableFlow`
  private def gameFlow(
    participantId: ParticipantId,
    clientId: ClientId,
    logicIn: TableFlowSink,
    logicOut: TableFlowSource
  ): Flow[MessageToServer, MessageToClient, NotUsed] = {

    val sink = Flow[MessageToServer]
      .map(IncomingAction.IncomingMessage(participantId, _))
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

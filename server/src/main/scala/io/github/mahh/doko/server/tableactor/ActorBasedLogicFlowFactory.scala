package io.github.mahh.doko.server.tableactor

import akka.NotUsed
import akka.actor.typed.scaladsl.ActorContext
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.typed.scaladsl.ActorSink
import akka.stream.typed.scaladsl.ActorSource
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.server.LogicFlowFactory
import io.github.mahh.doko.server.Routes
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer

class ActorBasedLogicFlowFactory(parentContext: ActorContext[_])(using rules: Rules)
  extends LogicFlowFactory {

  private val tableActor = parentContext.spawn(TableActor.behavior, "TableActor")
  override def flowForParticipant(
    id: ParticipantId
  ): Flow[MessageToServer, MessageToClient, NotUsed] = {

    val sink = Flow[MessageToServer]
      .map(msg => IncomingAction.IncomingMessageFromClient(id, msg))
      .to(
        ActorSink.actorRef(
          tableActor,
          IncomingAction.ClientLeaving(id, None),
          e => IncomingAction.ClientLeaving(id, Some(e))
        )
      )

    val source = ActorSource
      .actorRef[OutgoingAction](
        { case OutgoingAction.Completed => () },
        PartialFunction.empty,
        ActorBasedLogicFlowFactory.OutgoingBufferSize,
        OverflowStrategy.fail
      )
      .mapMaterializedValue { ref =>
        tableActor ! IncomingAction.ClientJoined(id, ref)
      }
      .collect { case OutgoingAction.NewMessageToClient(s) =>
        s
      }

    Flow.fromSinkAndSourceCoupled(sink, source)
  }

}

object ActorBasedLogicFlowFactory {
  private val OutgoingBufferSize = 16
}

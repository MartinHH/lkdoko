package io.github.mahh.doko.server.tableactor

import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.logic.table.IncomingAction.ClientJoined
import io.github.mahh.doko.logic.table.IncomingAction.IncomingMessage
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.server.LogicFlowFactory
import io.github.mahh.doko.server.tableactor.IncomingTableActorMessage.WrappedIncomingAction
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer
import org.apache.pekko.NotUsed
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.Flow
import org.apache.pekko.stream.typed.scaladsl.ActorSink
import org.apache.pekko.stream.typed.scaladsl.ActorSource

class ActorBasedLogicFlowFactory(parentContext: ActorContext[?])(using rules: Rules)
  extends LogicFlowFactory {

  private val tableActor = parentContext.spawn(TableActor.behavior, "TableActor")
  override def flowForParticipant(
    id: ParticipantId
  ): Flow[MessageToServer, MessageToClient, NotUsed] = {

    val sink = Flow[MessageToServer]
      .map(msg => WrappedIncomingAction(IncomingMessage(id, msg)))
      .to(
        ActorSink.actorRef(
          tableActor,
          IncomingTableActorMessage.ClientLeaving(id, None),
          e => IncomingTableActorMessage.ClientLeaving(id, Some(e))
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
        tableActor ! WrappedIncomingAction(ClientJoined(ref, id))
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

package io.github.mahh.doko.server

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.Flow
import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.msg.MessageToClient
import io.github.mahh.doko.shared.msg.MessageToServer

trait LogicFlowFactory {
  def flowForParticipant(
    id: ParticipantId
  ): Flow[MessageToServer, MessageToClient, NotUsed]
}

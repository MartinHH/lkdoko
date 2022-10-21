package io.github.mahh.doko.server.tableflow

import io.github.mahh.doko.logic.table.participant.ParticipantId
import io.github.mahh.doko.shared.msg.MessageToServer

private trait IncomingAction

private object IncomingAction:
  case class ClientJoined(
    clientId: ClientId,
    participantId: ParticipantId
  ) extends IncomingAction

  case class IncomingMessage(
    participantId: ParticipantId,
    msg: MessageToServer
  ) extends IncomingAction

  case class ClientLeft(
    clientId: ClientId,
    participantId: ParticipantId
  ) extends IncomingAction

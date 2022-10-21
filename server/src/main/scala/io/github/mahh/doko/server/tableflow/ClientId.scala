package io.github.mahh.doko.server.tableflow

private opaque type ClientId = java.util.UUID

private object ClientId:
  def random(): ClientId = java.util.UUID.randomUUID()

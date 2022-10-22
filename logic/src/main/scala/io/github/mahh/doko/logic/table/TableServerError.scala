package io.github.mahh.doko.logic.table

/**
 * Errors that may occur during handling of `IncomingAction`s.
 */
sealed trait TableServerError

object TableServerError:

  sealed trait PlayerActionError extends TableServerError

  case object NonExistingPlayer extends PlayerActionError

  case object PlayersIncomplete extends PlayerActionError

  case object ActionNotApplicable extends PlayerActionError

  sealed trait ClientLeftError extends TableServerError

  case object UnknownClient extends ClientLeftError

package io.github.mahh.doko.http4sserver.utils.logging

import io.github.mahh.doko.shared.utils.logging.LogLevel.*
import io.github.mahh.doko.shared.utils.logging.LogTask
import org.typelevel.log4cats.MessageLogger

extension (logTask: LogTask)
  def asLog4Cats[F[_]: MessageLogger]: F[Unit] =
    val logger = MessageLogger[F]
    logTask.level match
      case Trace => logger.trace(logTask.content())
      case Debug => logger.debug(logTask.content())
      case Info  => logger.info(logTask.content())
      case Warn  => logger.warn(logTask.content())
      case Error => logger.error(logTask.content())

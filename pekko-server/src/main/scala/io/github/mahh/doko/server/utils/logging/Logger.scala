package io.github.mahh.doko.server.utils.logging

import io.github.mahh.doko.shared.utils.logging.LogLevel.*
import io.github.mahh.doko.shared.utils.logging.LogTask
import org.slf4j.Logger

extension (logger: Logger)
  def apply(logTask: LogTask): Unit =
    val log: Logger => String => Unit = logTask.level match
      case Trace => _.trace
      case Debug => _.debug
      case Info  => _.info
      case Warn  => _.warn
      case Error => _.error
    log(logger)(logTask.content())

package io.github.mahh.doko.shared.utils.logging

enum LogLevel:
  case Trace, Debug, Info, Warn, Error

  def apply(msg: => String): LogTask = LogTask(this, () => msg)

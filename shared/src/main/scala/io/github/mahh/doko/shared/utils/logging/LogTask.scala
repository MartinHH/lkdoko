package io.github.mahh.doko.shared.utils.logging

case class LogTask(level: LogLevel, content: () => String)

package io.github.mahh.doko.http4sserver.utils.logging

import cats.effect.Sync
import org.typelevel.log4cats.LoggerName
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object LoggingCompanion {
  def getLogger[F[_]](implicit f: Sync[F], name: LoggerName): SelfAwareStructuredLogger[F] =
    Slf4jLogger.getLogger[F]
}

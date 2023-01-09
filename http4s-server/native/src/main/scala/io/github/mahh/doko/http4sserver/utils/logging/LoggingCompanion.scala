package io.github.mahh.doko.http4sserver.utils.logging

import cats.effect.Sync
import org.typelevel.log4cats.LoggerName
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.noop.NoOpLogger

object LoggingCompanion {
  def getLogger[F[_]](implicit f: Sync[F], name: LoggerName): SelfAwareStructuredLogger[F] =
    NoOpLogger[F]
}

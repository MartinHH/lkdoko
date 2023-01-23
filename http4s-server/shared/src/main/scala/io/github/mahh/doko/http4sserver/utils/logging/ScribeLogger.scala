package io.github.mahh.doko.http4sserver.utils.logging

import cats.effect.Sync
import org.typelevel.log4cats
import org.typelevel.log4cats.LoggerName
import scribe.Scribe
import scribe.cats.*

class ScribeLogger[F[_]: Sync](private val scribeLogger: scribe.Logger)
  extends log4cats.Logger[F] {

  private val scribe: Scribe[F] = scribeLogger.f[F]

  def error(message: => String): F[Unit] = scribe.error(message)

  def warn(message: => String): F[Unit] = scribe.warn(message)

  def info(message: => String): F[Unit] = scribe.info(message)

  def debug(message: => String): F[Unit] = scribe.debug(message)

  def trace(message: => String): F[Unit] = scribe.trace(message)

  def error(t: Throwable)(message: => String): F[Unit] = scribe.error(message, t)

  def warn(t: Throwable)(message: => String): F[Unit] = scribe.warn(message, t)

  def info(t: Throwable)(message: => String): F[Unit] = scribe.info(message, t)

  def debug(t: Throwable)(message: => String): F[Unit] = scribe.debug(message, t)

  def trace(t: Throwable)(message: => String): F[Unit] = scribe.trace(message, t)
}

object ScribeLogger:
  def getLogger[F[_]](implicit f: Sync[F], name: LoggerName) =
    new ScribeLogger[F](scribe.Logger(name.value))
package io.github.mahh.doko.http4sserver.utils.logging

import cats.syntax.flatMap.catsSyntaxIfM
import cats.effect.Sync
import org.typelevel.log4cats
import org.typelevel.log4cats.LoggerName
import scribe.Level
import scribe.Scribe
import scribe.cats.*
import scribe.format.*
import scribe.mdc.MDC
import scribe.mdc.MDCValue

class ScribeLogger[F[_]: Sync](private val scribeLogger: scribe.Logger)
  extends log4cats.SelfAwareStructuredLogger[F] {

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

  // starting here: SelfAwareLogger implementation
  // (not really used by http4s, but required by LoggerFactory type signature)

  private def isLevelEnabled(level: Level): F[Boolean] =
    Sync[F].delay(scribeLogger.includes(level))

  def isTraceEnabled: F[Boolean] = isLevelEnabled(Level.Trace)

  def isDebugEnabled: F[Boolean] = isLevelEnabled(Level.Debug)

  def isInfoEnabled: F[Boolean] = isLevelEnabled(Level.Info)

  def isWarnEnabled: F[Boolean] = isLevelEnabled(Level.Warn)

  def isErrorEnabled: F[Boolean] = isLevelEnabled(Level.Error)

  // starting here: StructuredLogger implementation (not really used by http4s)
  // (not really used by http4s, but required by LoggerFactory type signature)

  private[this] def contextLog(
    isEnabled: F[Boolean],
    ctx: Map[String, String],
    logging: () => Unit
  ): F[Unit] = {
    val ifEnabled = Sync[F].delay {
      MDC.context(ctx.map { (k, v) => k -> MDCValue(() => v) }.toSeq*) {
        logging()
      }
    }

    isEnabled.ifM(
      ifEnabled,
      Sync[F].unit
    )
  }

  def trace(ctx: Map[String, String])(msg: => String): F[Unit] =
    contextLog(isTraceEnabled, ctx, () => scribeLogger.trace(msg))

  def trace(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    contextLog(isTraceEnabled, ctx, () => scribeLogger.trace(msg, t))

  def debug(ctx: Map[String, String])(msg: => String): F[Unit] =
    contextLog(isDebugEnabled, ctx, () => scribeLogger.debug(msg))

  def debug(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    contextLog(isDebugEnabled, ctx, () => scribeLogger.debug(msg, t))

  def info(ctx: Map[String, String])(msg: => String): F[Unit] =
    contextLog(isInfoEnabled, ctx, () => scribeLogger.info(msg))

  def info(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    contextLog(isInfoEnabled, ctx, () => scribeLogger.info(msg, t))

  def warn(ctx: Map[String, String])(msg: => String): F[Unit] =
    contextLog(isWarnEnabled, ctx, () => scribeLogger.warn(msg))

  def warn(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    contextLog(isWarnEnabled, ctx, () => scribeLogger.warn(msg, t))

  def error(ctx: Map[String, String])(msg: => String): F[Unit] =
    contextLog(isErrorEnabled, ctx, () => scribeLogger.error(msg))

  def error(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    contextLog(isErrorEnabled, ctx, () => scribeLogger.error(msg, t))
}

object ScribeLogger:

  // do not use default formatter because it tries to print source location which is useless
  // because the ScribeLogger wrapper cannot forward the proper sourcecode implicits
  private val formatter: Formatter =
    formatter"[$date $threadName $level] $messages"

  // install the formatter globally (this is ugly, but so far, I did not find a better
  // way
  scribe.Logger.root
    .clearHandlers()
    .withHandler(formatter = formatter)
    .replace()

  def getLogger[F[_]](
    implicit f: Sync[F],
    name: LoggerName
  ): log4cats.SelfAwareStructuredLogger[F] =
    new ScribeLogger[F](scribe.Logger(name.value))

  trait LoggerFactory[F[_]: Sync] extends log4cats.LoggerFactory[F]:

    def getLoggerFromName(name: String): LoggerType =
      given LoggerName = LoggerName(name)
      ScribeLogger.getLogger

    def fromName(name: String): F[LoggerType] =
      Sync[F].delay(getLoggerFromName(name))

  def factory[F[_]: Sync]: LoggerFactory[F] = new LoggerFactory[F] {}

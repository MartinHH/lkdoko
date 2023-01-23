package io.github.mahh.doko.http4sserver.resources

import cats.data.OptionT
import cats.effect.Sync
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile

object ResourceFiles:
  def fromResource[F[_]: Sync](name: String, req: Option[Request[F]]): OptionT[F, Response[F]] =
    StaticFile.fromResource(name, req)

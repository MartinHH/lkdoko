package io.github.mahh.doko.http4sserver.resources

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all._
import fs2.io.file.Files
import fs2.io.readInputStream
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.Entity
import org.http4s.StaticFile.nameToContentType
import org.http4s.headers._
import org.http4s.syntax.header._
import org.http4s.MediaType
import org.http4s.Headers

import java.io.InputStream

object ResourceFiles:
  // TODO: this is just a quick hack modelled after org.http4s.StaticFile, but certainly missing
  //  some stuff...
  def fromResource[F[_]: Sync](name: String, req: Option[Request[F]]): OptionT[F, Response[F]] =
    OptionT(Sync[F].blocking {
      Option(getClass.getResourceAsStream(name)).map { inputStream =>
        val contentType = nameToContentType(name)
        val headers =
          Headers(
            contentType
          )
        Response(
          headers = headers,
          entity =
            Entity(readInputStream[F](Sync[F].pure(inputStream), StaticFile.DefaultBufferSize))
        )
      }
    })

  private def nameToContentType(name: String): Option[`Content-Type`] =
    name.lastIndexOf('.') match {
      case -1 => None
      case i  => MediaType.forExtension(name.substring(i + 1)).map(`Content-Type`(_))
    }

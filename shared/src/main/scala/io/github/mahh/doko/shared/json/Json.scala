package io.github.mahh.doko.shared.json

import scala.deriving.Mirror

/**
 * Wraps current json codec implementation (circe) to make it easier to change it.
 *
 * The future of circe seems to be uncertain (especially for scala 3), so this shall ensure it will be rather simple to
 * replace it.
 */
object Json {

  type Decoder[A] = io.circe.Decoder[A]

  type Encoder[A] = io.circe.Encoder[A]

  // decorate the companions to enable "derives Encoder, Decoder" syntax:

  extension (companion: io.circe.Decoder.type)
    inline def derived[A](using m: Mirror.Of[A]): Decoder[A] = io.circe.generic.semiauto.deriveDecoder

  extension (companion: io.circe.Encoder.type)
    inline def derived[A](using m: Mirror.Of[A]): Encoder[A] = io.circe.generic.semiauto.deriveEncoder

  type DecodeError = io.circe.Error

  def decode[A: Decoder](json: String): Either[DecodeError, A] = {
    io.circe.parser.decode(json)
  }

  def encode[A: Encoder](a: A): String = {
    import io.circe.syntax.EncoderOps
    a.asJson.noSpaces
  }
}

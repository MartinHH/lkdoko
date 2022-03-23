package io.github.mahh.doko.shared.json

/**
 * Wraps current json codec implementation (circe) to make it easier to change it.
 *
 * The future of circe seems to be uncertain (especially for scala 3), so this shall ensure it will be rather simple to
 * replase it.
 */
object Json {

  type Decoder[A] = io.circe.Decoder[A]

  type Encoder[A] = io.circe.Encoder[A]

  type DecodeError = io.circe.Error

  def decode[A: Decoder](json: String): Either[DecodeError, A] = {
    io.circe.parser.decode(json)
  }

  def encode[A: Encoder](a: A): String = {
    import io.circe.syntax.EncoderOps
    a.asJson.noSpaces
  }
}

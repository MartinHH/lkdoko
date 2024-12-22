package io.github.mahh.doko.shared.json

/**
 * Wraps current json codec implementation (circe) to make it easier to change it.
 *
 * The future of circe seems to be uncertain (especially for scala 3), so this shall ensure it will be rather simple to
 * replace it.
 */
object Json extends io.circe.generic.AutoDerivation {

  type Decoder[A] = io.circe.Decoder[A]

  type Encoder[A] = io.circe.Encoder[A]

  def Decoder: io.circe.Decoder.type = io.circe.Decoder

  def Encoder: io.circe.Encoder.type = io.circe.Encoder

  type DecodeError = io.circe.Error

  def decode[A: Decoder](json: String): Either[DecodeError, A] = {
    io.circe.parser.decode(json)
  }

  def encode[A: Encoder](a: A): String = {
    import io.circe.syntax.EncoderOps
    a.asJson.noSpaces
  }
}

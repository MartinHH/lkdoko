package io.github.mahh.doko.shared.utils

extension [A](opt: Option[A])
  def toEither[E](ifEmpty: => E): Either[E, A] =
    opt.fold(Left(ifEmpty))(Right.apply)

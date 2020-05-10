package io.github.mahh.doko.shared.utils

object OrderingFromSeq {

  def apply[T](seq: Seq[T]): Ordering[T] = {
    val lookup: T => Int = {
      val withoutDefault = seq.zipWithIndex.toMap
      val default = seq.size
      withoutDefault.withDefaultValue(default)
    }
    Ordering.by(lookup)
  }
}

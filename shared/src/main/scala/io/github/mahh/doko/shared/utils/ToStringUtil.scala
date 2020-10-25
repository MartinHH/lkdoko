package io.github.mahh.doko.shared.utils

object ToStringUtil {

  /**
   * Creates strings similar to the default toString of case classes.
   */
  def productToString(p: Product): String = {
    // probably not the most efficient, but does the trick.
    s"${p.getClass.getSimpleName}(${p.productIterator.mkString(",")})"
  }

}

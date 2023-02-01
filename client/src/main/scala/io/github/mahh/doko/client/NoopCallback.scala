package io.github.mahh.doko.client

/**
 * Dummy `() => Unit` (as case object for equality checks and toString for debugging).
 */
case object NoopCallback extends (() => Unit):
  def apply(): Unit = ()

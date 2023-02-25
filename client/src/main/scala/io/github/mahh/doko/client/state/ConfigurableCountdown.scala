package io.github.mahh.doko.client.state

import com.raquo.airstream.core.*

object ConfigurableCountdown {

  def countDown(
    active: Signal[Boolean],
    timeout: Signal[Option[Int]]
  ): Signal[Option[Int]] = {
    val activeTimeOut: Signal[Option[Int]] =
      active.combineWithFn(timeout) { (a, to) =>
        to.filter(_ => a)
      }
    activeTimeOut.changes
      .flatMap {
        case Some(to) =>
          EventStream.periodic(1000).map { tick =>
            Some(math.max(to - tick, 0))
          }
        case None =>
          EventStream.fromValue(None)
      }
      .toSignal(None)
  }

}

package io.github.mahh.doko.client.state

import com.raquo.airstream.core.*

object ConfigurableCountdown {

  def countDown(
    activeTimeOut: Signal[Option[Int]],
    intervalMs: Int = 1000
  ): Signal[Option[Int]] = {
    activeTimeOut.toObservable
      .flatMapSwitch {
        case Some(to) =>
          EventStream.periodic(intervalMs, resetOnStop = true).map { tick =>
            Some(math.max(to - tick, 0))
          }
        case None =>
          EventStream.fromValue(None)
      }
      .toSignal(None)
      .distinct
  }

}

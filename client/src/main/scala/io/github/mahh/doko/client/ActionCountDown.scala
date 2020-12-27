package io.github.mahh.doko.client

import scala.concurrent.duration.DurationInt
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetIntervalHandle

/**
 * Timer-controlled countdown that executes an action when 0 has been reached.
 */
class ActionCountDown(isActive: => Boolean, defaultWait: Int) {
  private var timeoutHandle: Option[SetIntervalHandle] = None
  private var countDownCallbacks: Set[Int => Unit] = Set.empty
  private var countDown = 0

  def clear(): Unit = {
    timeoutHandle.foreach(timers.clearInterval)
    countDownCallbacks = Set.empty
    countDown = 0
  }

  def startCountdown(
    onFinished: () => Unit,
    seconds: Int = defaultWait
  ): Unit = if (isActive) {
    countDown = seconds

    val handle = {
      timers.setInterval(1.second) {
        countDown -= 1
        if (countDown <= 0) {
          if (isActive) {
            onFinished()
          }
        } else {
          countDownCallbacks.foreach(_.apply(countDown))
        }
      }
    }
    timeoutHandle = Some(handle)
  }

  def addCountDownCallback(callback: Int => Unit): Unit = {
    countDownCallbacks += callback
    if (timeoutHandle.nonEmpty) {
      callback(countDown)
    }
  }
}

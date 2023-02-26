package io.github.mahh.doko.client.state

import com.raquo.airstream.core.Signal
import io.github.mahh.doko.client.testutils.AirstreamSuite

import scala.concurrent.Promise

class ConfigurableCountdownSpec extends AirstreamSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  testWithOwner("Counts down to Some(0) and stops if config is non-empty") {
    val countdown = ConfigurableCountdown.countDown(Signal.fromValue(Some(3)), 50)
    var x = Vector.empty[Option[Int]]
    countdown.foreach(x :+= _)
    scheduleFuture(300).map { _ =>
      assertEquals(x, Vector(None, Some(3), Some(2), Some(1), Some(0)))
    }
  }

  testWithOwner("Stays at None if config is empty") {
    val countdown = ConfigurableCountdown.countDown(Signal.fromValue(None), 50)
    var x = Vector.empty[Option[Int]]
    countdown.foreach(x :+= _)
    scheduleFuture(300).map { _ =>
      assertEquals(x, Vector(None))
    }
  }
}

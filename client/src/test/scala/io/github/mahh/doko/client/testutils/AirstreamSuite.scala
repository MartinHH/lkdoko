package io.github.mahh.doko.client.testutils

import com.raquo.airstream.core.Signal
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.ownership.Owner
import munit.FunSuite
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Prop

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js

abstract class AirstreamSuite extends ScalaCheckSuite {

  protected def testWithOwner(name: String)(body: Owner ?=> Any)(using Location): Unit = {
    given owner: ManualOwner = new ManualOwner

    test(name)(body)
    owner.killSubscriptions()
  }

  protected def propWithOwner(name: String)(body: Owner ?=> Prop)(using Location): Unit = {
    given owner: ManualOwner = new ManualOwner

    property(name)(body)
    owner.killSubscriptions()
  }

  protected def scheduleFuture(delayMs: Int)(using Owner): Future[Unit] = {
    val promise = Promise[Unit]()
    js.timers.setTimeout(delayMs) {
      promise.success(())
    }
    promise.future
  }

  protected def currentValueProp[T](signal: Signal[T])(prop: T => Prop)(using Owner): Prop = {
    var tOpt = Option.empty[T]
    signal.foreach(t => tOpt = Some(t)).kill()
    tOpt.fold(Prop.falsified :| "No value from signal")(prop)
  }
}

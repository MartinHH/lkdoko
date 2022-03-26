package io.github.mahh.doko.shared.game

import munit.FunSuite

class ReservationSpec extends FunSuite {

  // this is rather obvious, but somehow null-values appear(ed) to sneak into other tests
  test("Solo.solo of Solo.all must not be null") {
    val all = Reservation.Solo.All
    assert(all.forall(_.solo != null), s"No null in $all")
  }

}

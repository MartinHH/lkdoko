package io.github.mahh.doko.logic.table

import io.github.mahh.doko.shared.table.TableMap
import org.scalacheck.Gen

// TODO: move this to shared if we ever get scala-js-support by scalacheck

object TableMapGens {

  def flatMappedTableMapGen[A, B](
    tableMap: TableMap[A],
    f: A => Gen[B]
  ): Gen[TableMap[B]] =
    for {
      b1 <- f(tableMap.player1Val)
      b2 <- f(tableMap.player2Val)
      b3 <- f(tableMap.player3Val)
      b4 <- f(tableMap.player4Val)
    } yield TableMap(b1, b2, b3, b4)
}

package io.github.mahh.doko.shared.table

import org.scalacheck.Gen

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

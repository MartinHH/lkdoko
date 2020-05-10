package io.github.mahh.doko.shared.deck

sealed abstract class Rank(val value: Int, val shortName: String)

object Rank {

  case object A extends Rank(11, "A")

  case object Ten extends Rank(10, "10")

  case object K extends Rank(4, "K")

  case object Q extends Rank(3, "Q")

  case object J extends Rank(2, "J")

  case object Nine extends Rank(0, "9")

  type A = A.type
  type Ten = Ten.type
  type K = K.type
  type Q = Q.type
  type J = J.type
  type Nine = Nine.type

  val all: List[Rank] = List(A, Ten, K, Q, J, Nine)
}

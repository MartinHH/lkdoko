package io.github.mahh.doko.shared.score

import cats.instances.all._
import cats.syntax.all._
import io.github.mahh.doko.shared.player.PlayerPosition

/**
 * History of scores.
 *
 * @param scores The scores since start of the game (in reverse / "newest score first" order).
 */
case class TotalScores(scores: List[Scores]) {
  def addScores(additionalScores: Scores): TotalScores = copy(scores = additionalScores :: scores)

  def sumPerPlayer: Map[PlayerPosition, Int] = scores.foldMap(_.totalsPerPlayer)
}

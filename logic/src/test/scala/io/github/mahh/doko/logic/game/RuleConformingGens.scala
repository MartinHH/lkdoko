package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.testutils.GenUtils
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

/**
 * Special `Gen`s that generate data that sticks to the game's rules.
 */
object RuleConformingGens {

  import io.github.mahh.doko.logic.testutils.DeriveArbitrary._

  val shuffledPackGen: Gen[List[Card]] = GenUtils.shuffle(Card.fullPack)

  /**
   * Generates (scalacheck-)randomly shuffled pack with 4x12 cards as according to the rules.
   */
  val dealtCardsGen: Gen[TableMap[List[Card]]] = shuffledPackGen.map(Dealer.dealtCards)

  private[game] def negotiatingGen(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = Gen.const(TotalScores(List.empty)),
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  ): Gen[FullGameState.Negotiating] =
    for {
      sp <- startingPlayerGen
      ts <- totalScoresGen
      dc <- dealtCardsGen
    } yield FullGameState.Negotiating.withDealtCards(sp, ts, dc)
}

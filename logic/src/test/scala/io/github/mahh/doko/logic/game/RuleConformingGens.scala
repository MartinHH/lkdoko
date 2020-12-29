package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.table.TableMapGens
import io.github.mahh.doko.shared.testutils.GenUtils
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

/**
 * Special `Gen`s that generate data that sticks to the game's rules.
 */
object RuleConformingGens {

  import io.github.mahh.doko.shared.testutils.DeriveArbitrary._

  val shuffledPackGen: Gen[List[Card]] = GenUtils.shuffle(Card.fullPack)

  /**
   * Generates (scalacheck-)randomly shuffled pack with 4x12 cards as according to the rules.
   */
  val dealtCardsGen: Gen[TableMap[List[Card]]] = shuffledPackGen.map(Dealer.dealtCards)

  // TODO: make this more arbitrary (while keeping it "rule conforming"):
  val totalScoresGen: Gen[TotalScores] = Gen.const(TotalScores(List.empty))

  /**
   * Generates a valid initial `FullGameState.Negotiating`.
   */
  private[game] def negotiatingGen(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  ): Gen[FullGameState.Negotiating] =
    for {
      sp <- startingPlayerGen
      ts <- totalScoresGen
      dc <- dealtCardsGen
    } yield FullGameState.Negotiating.withDealtCards(sp, ts, dc)


  /**
   * Generates via `negotiatingGen` and then executes four valid `CallReservation` calls (one for each player).
   *
   * @note This should always generate `Some(state: FullGameState.NegotiationsResult)`. See `negotiationsResultGen`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def negotiatingAfterFourValidCallsGen(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  ): Gen[Option[FullGameState]] = {

    def genValidCall(s: Negotiating.PlayerState): Gen[PlayerAction.CallReservation] = {
      val all: Seq[Option[Reservation]] = None +: s.reservationState.fold(_.map(Option.apply), _ => Seq.empty)
      Gen.oneOf(all).map(PlayerAction.CallReservation)
    }

    for {
      neg <- RuleConformingGens.negotiatingGen(startingPlayerGen, totalScoresGen, dealtCardsGen)
      calls <- TableMapGens.flatMappedTableMapGen(neg.players, genValidCall)
    } yield {
      calls.toMap.foldLeft(Option[FullGameState](neg)) { (stateOpt, action) =>
        stateOpt.flatMap(_.handleAction.lift(action))
      }
    }
  }

  private[game] def negotiationsResultGen(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  ): Gen[FullGameState.NegotiationsResult] = {
    negotiatingAfterFourValidCallsGen(startingPlayerGen, totalScoresGen, dealtCardsGen)
      .suchThat(_.exists(_.isInstanceOf[FullGameState.NegotiationsResult]))
      .map(_.get.asInstanceOf[FullGameState.NegotiationsResult])
  }

  private[game] def playingGen(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  ): Gen[FullGameState.Playing] = {
    // TODO: improve this - currently, it only generates playing-states without any reservations
    import io.github.mahh.doko.logic.game.FullGameStateSpec.RichFullGameState.Implicits._
    negotiatingGen(startingPlayerGen, totalScoresGen, dealtCardsGen).map { neg =>
      neg
        .applyActionForAllPLayers(PlayerAction.CallReservation(None))
        .applyActionForAllPLayers(PlayerAction.AcknowledgeReservation)
    }.suchThat(_.isInstanceOf[FullGameState.Playing]).map(_.asInstanceOf[FullGameState.Playing])
  }

}

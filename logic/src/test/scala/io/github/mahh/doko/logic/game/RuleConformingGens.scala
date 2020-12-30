package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.Acknowledgement
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.table.TableMapGens
import io.github.mahh.doko.shared.testutils.GenUtils
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import scala.reflect.ClassTag

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
   * Gens used to create an initial `FullGameState`.
   */
  case class InitialGens(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[List[Card]]] = dealtCardsGen
  )

  private def withShuffledActionsGen(
    fullGameState: FullGameState,
    calls: TableMap[_ <: PlayerAction[_ <: GameState]]
  ): Gen[Option[FullGameState]] = {
    for {
      players <- GenUtils.shuffle(calls.toMap.toList)
    } yield {
      players.foldLeft(Option[FullGameState](fullGameState)) { case (stateOpt, (pos, action)) =>
        stateOpt.flatMap(_.handleAction.lift(pos, action))
      }
    }
  }

  private def acknowledgedGen(
    fullGameStateGen: Gen[FullGameState],
    acknowledgement: Acknowledgement[_ <: GameState]
  ): Gen[Option[FullGameState]] = {
    for {
      state <- fullGameStateGen
      stateOpt <- withShuffledActionsGen(state, TableMap.fill(acknowledgement))
    } yield stateOpt
  }

  private def collectSomeState[State <: FullGameState: ClassTag](
    fullGameStateOptGen: Gen[Option[FullGameState]]
  ): Gen[State] = fullGameStateOptGen.suchThat {
    case Some(_: State) => true
    case _ => false
  }.map(_.get.asInstanceOf[State])

  private def validReservationGen(
    s: Negotiating.PlayerState,
    reservationFilter: Reservation => Boolean = _ => true
  ): Gen[PlayerAction.CallReservation] = {
    val all: Seq[Option[Reservation]] =
      None +: s.reservationState.fold(_.filter(reservationFilter).map(Option.apply), _ => Seq.empty)
    Gen.oneOf(all).map(PlayerAction.CallReservation)
  }

  /**
   * Generates a valid initial `FullGameState.Negotiating`.
   */
  private[game] def negotiatingGen(
    gens: InitialGens = InitialGens()
  ): Gen[FullGameState.Negotiating] = {
    for {
      sp <- gens.startingPlayerGen
      ts <- gens.totalScoresGen
      dc <- gens.dealtCardsGen
    } yield FullGameState.Negotiating.withDealtCards(sp, ts, dc)
  }


  /**
   * Generates via `negotiatingGen` and then executes four valid `CallReservation` calls (one for each player).
   *
   * @note This should always generate `Some(state: FullGameState.NegotiationsResult)`. See `negotiationsResultGen`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def negotiatingAfterFourValidReservationsGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: Reservation => Boolean = _ => true
  ): Gen[Option[FullGameState]] = {
    for {
      neg <- RuleConformingGens.negotiatingGen(gens)
      calls <- TableMapGens.flatMappedTableMapGen(neg.players, validReservationGen(_, reservationFilter))
      stateOpt <- withShuffledActionsGen(neg, calls)
    } yield stateOpt
  }

  private[game] def negotiationsResultGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: Reservation => Boolean = _ => true
  ): Gen[FullGameState.NegotiationsResult] = {
    collectSomeState[FullGameState.NegotiationsResult](
      negotiatingAfterFourValidReservationsGen(gens, reservationFilter)
    )
  }

  /**
   * Generates via `negotiationsResultGen` and then executes the four expected acknowledgments.
   *
   * @note This should always generate `Some(state: FullGameState)` where `state` is one of the
   *       valid follow-up states of `NegotiationsResult`. See `negotiationsResultFollowUpGen`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def acknowledgedNegotiationsResultGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: Reservation => Boolean = _ => true
  ): Gen[Option[FullGameState]] = {
    acknowledgedGen(
      negotiationsResultGen(gens, reservationFilter),
      PlayerAction.AcknowledgeReservation
    )
  }

  private[game] def negotiationsResultFollowUpGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: Reservation => Boolean = _ => true
  ): Gen[FullGameState] = {
    collectSomeState[FullGameState](
      acknowledgedNegotiationsResultGen(gens, reservationFilter)
    )
  }

  private[game] def playingGen(
    gens: InitialGens = InitialGens()
  ): Gen[FullGameState.Playing] = {
    // TODO: include "poverty" - this currently does not generate Playing-states resulting
    //  from a player calling "poverty".
    collectSomeState[FullGameState.Playing](
      acknowledgedNegotiationsResultGen(
        gens = gens,
        reservationFilter = r => r != Reservation.Throwing && r != Reservation.Poverty
      )
    )
  }

}

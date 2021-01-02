package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game
import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.deck.QueenOfClubs
import io.github.mahh.doko.shared.deck.Rank
import io.github.mahh.doko.shared.game.CardsPerPlayer
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

  object Dealer {

    /**
     * Generates (scalacheck-)randomly shuffled pack with 4x12 cards as according to the rules.
     */
    val simpleGen: Gen[TableMap[Seq[Card]]] = shuffledPackGen.map(pack => game.Dealer.dealtCards(pack))

    type HandSelector = (Int, Card => Boolean)

    /**
     * Generates 4x12 cards where one player's hand is determined by applying the selectors.
     */
    def withSpecialHandForOnePlayer(
      selectors: HandSelector*
    ): Gen[(PlayerPosition, TableMap[Seq[Card]])] = {

      def applySelectors(deck: List[Card]): (Vector[Card], List[Card]) = {
        selectors.foldLeft[(Vector[Card], List[Card])](Vector.empty -> deck) {
          case ((selected, remaining), _) if selected.size >= CardsPerPlayer =>
            selected -> remaining
          case ((selected, remaining), (count, p)) =>
            val newlySelected = remaining.filter(p).take(math.min(count, CardsPerPlayer - selected.size))
            (selected ++ newlySelected) -> (remaining diff newlySelected)
        }
      }

      for {
        pack <- shuffledPackGen
        (selected, remainingPack) = applySelectors(pack)
        pos <- arbitrary[PlayerPosition]
      } yield {
        pos -> game.Dealer.dealtCards(
          remainingPack,
          TableMap.fill(Vector.empty[Card]) + (pos -> selected)
        )
      }
    }

    val povertyGen: Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      import io.github.mahh.doko.shared.rules.Trumps.Default.isTrump
      val povertySelectors: Seq[Seq[HandSelector]] =
        Seq(
          // poverty: either at most three trumps...
          Seq[HandSelector](
            (CardsPerPlayer - 3, !isTrump(_))
          ),
          // ... or four trumps where at least one is a fox
          Seq[HandSelector](
            (CardsPerPlayer - 4, !isTrump(_)),
            (1, _ == Fox)
          )
        )
      for {
        selectors <- Gen.oneOf(povertySelectors)
        result <- withSpecialHandForOnePlayer(selectors: _*)
      } yield result
    }

    val marriageGen: Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      withSpecialHandForOnePlayer((2, _ == QueenOfClubs))
    }

    val throwingGen: Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      val povertySelectors: Seq[Seq[HandSelector]] =
        Seq(
          // throwing: either at least five nines...
          Seq[HandSelector](
            (5, _.rank == Rank.Nine)
          ),
          // ... or at least five kings...
          Seq[HandSelector](
            (5, _.rank == Rank.K)
          ),
          // ... or at least four nines and four kings...
          Seq[HandSelector](
            (4, _.rank == Rank.Nine),
            (4, _.rank == Rank.K)
          ),
          // ... or at least seven with value >= 10
          Seq[HandSelector](
            (7, _.value >= Rank.Ten.value)
          )
        )
      for {
        selectors <- Gen.oneOf(povertySelectors)
        result <- withSpecialHandForOnePlayer(selectors: _*)
      } yield result
    }
  }

  // TODO: make this more arbitrary (while keeping it "rule conforming"):
  val totalScoresGen: Gen[TotalScores] = Gen.const(TotalScores(List.empty))

  type ReservationFilter = Option[Reservation] => Boolean

  object ReservationFilter {

    val soloOnly: ReservationFilter = _.exists {
      case _: Reservation.Solo => true
      case _ => false
    }

    private def not(reservation: Reservation): ReservationFilter = _.forall(_ != reservation)

    val notThrowing: ReservationFilter = not(Reservation.Throwing)

    val notPoverty: ReservationFilter = not(Reservation.Poverty)

    implicit class RichReservationFilter(private val f: ReservationFilter) extends AnyVal {
      def &&(that: ReservationFilter): ReservationFilter = r => f(r) && that(r)
    }
  }

  /**
   * Gens used to create an initial `FullGameState`.
   */
  case class InitialGens(
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = totalScoresGen,
    dealtCardsGen: Gen[TableMap[Seq[Card]]] = Dealer.simpleGen
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
    reservationFilter: ReservationFilter = _ => true
  ): Gen[PlayerAction.CallReservation] = {
    val all: Seq[Option[Reservation]] =
      (None +: s.reservationState.fold(_.map(Option.apply), _ => Seq.empty)).filter(reservationFilter)
    Gen.oneOf(all).map(PlayerAction.CallReservation)
  }

  private[game] def withSpecialHand[State <: FullGameState](
    handsGen: Gen[(PlayerPosition, TableMap[Seq[Card]])],
    stateGen: InitialGens => Gen[State],
    initialGensTemplate: InitialGens = InitialGens()
  ): Gen[(PlayerPosition, State)] = {
    for {
      (pos, hands) <- handsGen
      gens = initialGensTemplate.copy(dealtCardsGen = Gen.const(hands))
      state <- stateGen(gens)
    } yield pos -> state
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
    reservationFilter: ReservationFilter = _ => true
  ): Gen[Option[FullGameState]] = {
    for {
      neg <- RuleConformingGens.negotiatingGen(gens)
      calls <- TableMapGens.flatMappedTableMapGen(neg.players, validReservationGen(_, reservationFilter))
      stateOpt <- withShuffledActionsGen(neg, calls)
    } yield stateOpt
  }

  private[game] def negotiationsResultGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
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
    reservationFilter: ReservationFilter = _ => true
  ): Gen[Option[FullGameState]] = {
    acknowledgedGen(
      negotiationsResultGen(gens, reservationFilter),
      PlayerAction.AcknowledgeReservation
    )
  }

  private[game] def negotiationsResultFollowUpGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[FullGameState] = {
    collectSomeState[FullGameState](
      acknowledgedNegotiationsResultGen(gens, reservationFilter)
    )
  }

  /**
   * Generates a valid initial `FullGameState.Playing`.
   */
  private[game] def playingGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[FullGameState.Playing] = {
    // TODO: include "poverty" - this currently does not generate Playing-states resulting
    //  from a player calling "poverty".
    val filter: ReservationFilter = {
      import ReservationFilter._
      reservationFilter && notPoverty && notThrowing
    }
    collectSomeState[FullGameState.Playing](
      acknowledgedNegotiationsResultGen(
        gens = gens,
        reservationFilter = filter
      )
    )
  }

  /**
   * Generates via `playingGen` and then executes valid calls until `nCards` (or at most all) cards have been played.
   */
  private[game] def playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(
    nCards: Int = Card.fullPack.size + 1,
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[Option[FullGameState]] = {

    def play(playing: FullGameState.Playing, played: Int): Gen[Option[FullGameState]] = {

      def withAcknowledgements: Gen[(Option[FullGameState], Boolean)] = {
        val isLastTrick = playing.players.values.forall(_.hand.isEmpty)
        acknowledgedGen(Gen.const(playing), PlayerAction.AcknowledgeTrickResult).map(_ -> isLastTrick)
      }

      def genNextState: Gen[(Option[FullGameState], Boolean, Boolean)] = {
        val regularActionsGen: Gen[(Option[FullGameState], Boolean, Boolean)] = {
          playing.finishedTrickOpt.fold {
            // trick is being played - one player must be allowed to play a card:
            for {
              (pos, player) <- Gen.oneOf(playing.playerStates.filter { case (_, player) => player.canPlay.nonEmpty })
              card <- Gen.oneOf(player.canPlay)
            } yield {
              (playing.handleAction.lift(pos -> PlayerAction.PlayCard(card)), false, true)
            }
          } { _ =>
            // trick is done: acknowledge for all players:
            withAcknowledgements.map { case (stateOpt, ifFinished) => (stateOpt, ifFinished, false) }
          }
        }
        val minBids: Map[PlayerPosition, Bid] = playing.playerStates.collect {
          case (pos, state) if state.possibleBid.nonEmpty => pos -> state.possibleBid.get.bid
        }

        def bidGen: Gen[(Option[FullGameState], Boolean, Boolean)] =
          for {
            (pos, minBid) <- Gen.oneOf(minBids.toSeq)
            possibleBids = Bid.All.filter(Bid.ordering.gteq(_, minBid))
            bid <- Gen.oneOf(possibleBids)
          } yield (playing.handleAction.lift(pos, PlayerAction.PlaceBid(bid)), false, false)

        if (minBids.isEmpty) {
          regularActionsGen
        } else {
          Gen.frequency(
            30 -> regularActionsGen,
            1 -> bidGen
          )
        }
      }

      if (played >= nCards) {
        val directlyAborted = Gen.const(Some(playing))
        playing.finishedTrickOpt match {
          case None =>
            directlyAborted
          case Some(_) =>
            Gen.oneOf(directlyAborted, withAcknowledgements.map { case (stateOpt, _) => stateOpt })
        }
      } else {
        genNextState.flatMap {
          case (None, _, _) =>
            Gen.const(None)
          case (stateOpt, true, _) =>
            Gen.const(stateOpt)
          case (Some(state: FullGameState.Playing), _, wasCard) if wasCard =>
            play(state, played + 1)
          case (Some(state: FullGameState.Playing), _, _) =>
            play(state, played)
          case _ => Gen.fail
        }
      }

    }

    playingGen(gens, reservationFilter).flatMap(play(_, played = 0))
  }

  /**
   * Generates via `playingNCardsHaveBeenPlayedAndPossiblyAcknowledged` with at most `maxCardsPlayed` cards "played".
   *
   * @note This should always generate `Some(state: FullGameState.Playing)`. See `playingMidGame`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def playingAfterLessThanAllCardsHaveBeenPlayed(
    maxCardsPlayed: Int = Card.fullPack.size - 1,
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[Option[FullGameState]] = {
    for {
      nCards <- Gen.choose(0, math.min(maxCardsPlayed, Card.fullPack.size - 1))
      stateOpt <- playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(nCards, gens, reservationFilter)
    } yield stateOpt
  }

  private[game] def playingMidGame(
    maxCardsPlayed: Int = Card.fullPack.size - 1,
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[FullGameState.Playing] = {
    collectSomeState[FullGameState.Playing] {
      playingAfterLessThanAllCardsHaveBeenPlayed(maxCardsPlayed, gens, reservationFilter)
    }
  }

  /**
   * Generates via `playingGen` and then executes valid calls until all cards have been played.
   *
   * @note This should always generate `Some(state: FullGameState.RoundResults)`. See `roundResultsGen`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def playingAfterAllCardsHaveBeenPlayedAndAcknowledged(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[Option[FullGameState]] = {
    playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(gens = gens, reservationFilter = reservationFilter)
  }

  private[game] def roundResultsGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = _ => true
  ): Gen[FullGameState.RoundResults] = {
    collectSomeState[FullGameState.RoundResults](
      playingAfterAllCardsHaveBeenPlayedAndAcknowledged(gens, reservationFilter)
    )
  }

}

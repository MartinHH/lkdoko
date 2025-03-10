package io.github.mahh.doko.logic.game

import io.github.mahh.doko.logic.game
import io.github.mahh.doko.logic.game.FullGameState.Negotiating
import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.shared.bids.Bid
import io.github.mahh.doko.shared.deck.Card
import io.github.mahh.doko.shared.deck.Fox
import io.github.mahh.doko.shared.deck.QueenOfClubs
import io.github.mahh.doko.shared.deck.Rank
import io.github.mahh.doko.shared.game.GameState
import io.github.mahh.doko.shared.game.Reservation
import io.github.mahh.doko.shared.player.PlayerAction
import io.github.mahh.doko.shared.player.PlayerAction.Acknowledgement
import io.github.mahh.doko.shared.player.PlayerPosition
import io.github.mahh.doko.shared.score.TotalScores
import io.github.mahh.doko.shared.table.TableMap
import io.github.mahh.doko.shared.table.TableMapGens
import io.github.martinhh.derived.arbitrary.given
import io.github.mahh.doko.shared.testutils.GenUtils
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Special `Gen`s that generate data that sticks to the game's rules.
 */
object RuleConformingGens {

  implicit val rulesArb: Arbitrary[Rules] = Arbitrary(Gen.resultOf(Rules(_: DeckRule)))

  def shuffledPackGen(implicit deckRule: DeckRule): Gen[List[Card]] =
    GenUtils.shuffle(deckRule.fullPack)

  object Dealer {

    /**
     * Generates (scalacheck-)randomly shuffled pack with 4x12 cards as according to the rules.
     */
    def simpleGen(implicit deckRule: DeckRule): Gen[TableMap[Seq[Card]]] =
      shuffledPackGen.map(pack => game.Dealer.dealtCards(pack))

    type HandSelector = (Int, Card => Boolean)

    /**
     * Generates 4x12 cards where one player's hand is determined by applying the selectors.
     */
    def withSpecialHandForOnePlayer(
      selectors: HandSelector*
    )(
      implicit deckRule: DeckRule
    ): Gen[(PlayerPosition, TableMap[Seq[Card]])] = {

      def applySelectors(deck: List[Card]): (Vector[Card], List[Card]) = {
        selectors.foldLeft[(Vector[Card], List[Card])](Vector.empty -> deck) {
          case ((selected, remaining), _) if selected.size >= deckRule.cardsPerPlayer =>
            selected -> remaining
          case ((selected, remaining), (count, p)) =>
            val newlySelected =
              remaining.filter(p).take(math.min(count, deckRule.cardsPerPlayer - selected.size))
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

    def povertyGen(implicit deckRule: DeckRule): Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      import io.github.mahh.doko.shared.rules.Trumps.Default.isTrump
      val povertySelectors: Seq[Seq[HandSelector]] =
        Seq(
          // poverty: either at most three trumps...
          Seq[HandSelector](
            (deckRule.cardsPerPlayer - 3, !isTrump(_))
          ),
          // ... or four trumps where at least one is a fox
          Seq[HandSelector](
            (deckRule.cardsPerPlayer - 4, !isTrump(_)),
            (1, _ == Fox)
          )
        )
      for {
        selectors <- Gen.oneOf(povertySelectors)
        result <- withSpecialHandForOnePlayer(selectors*)
      } yield result
    }

    def marriageGen(implicit deckRule: DeckRule): Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      withSpecialHandForOnePlayer((2, _ == QueenOfClubs))
    }

    def throwingGen(implicit deckRule: DeckRule): Gen[(PlayerPosition, TableMap[Seq[Card]])] = {
      def withNinesOnly: Seq[Seq[(Int, Card => Boolean)]] = Seq(
        // ... or at least four nines and four kings...
        Seq[HandSelector](
          (4, _.rank == Rank.Nine),
          (4, _.rank == Rank.K)
        ),
        // throwing: either at least five nines...
        Seq[HandSelector](
          (5, _.rank == Rank.Nine)
        )
      )

      val povertySelectors: Seq[Seq[HandSelector]] =
        Seq(
          // ... or at least five kings...
          Seq[HandSelector](
            (5, _.rank == Rank.K)
          ),
          // ... or at least seven with value >= 10
          Seq[HandSelector](
            (7, _.value >= Rank.Ten.value)
          )
        ) ++ (if (deckRule == DeckRule.WithoutNines) Seq.empty else withNinesOnly)

      for {
        selectors <- Gen.oneOf(povertySelectors)
        result <- withSpecialHandForOnePlayer(selectors*)
      } yield result
    }
  }

  type ReservationFilter = Seq[Option[Reservation]] => Seq[Option[Reservation]]

  object ReservationFilter {

    def apply(p: Option[Reservation] => Boolean): ReservationFilter = _.filter(p)

    val neutral: ReservationFilter = identity

    val soloOnly: ReservationFilter = apply {
      _.exists {
        case _: Reservation.Solo => true
        case _                   => false
      }
    }

    private def not(reservation: Reservation): ReservationFilter = apply(_.forall(_ != reservation))

    val notThrowing: ReservationFilter = not(Reservation.Throwing)

    val notPoverty: ReservationFilter = not(Reservation.Poverty)

    val forcePoverty: ReservationFilter = reservations => {
      if (reservations.exists(_.contains(Reservation.Poverty)))
        Seq(Some(Reservation.Poverty))
      else
        reservations.filterNot(_.exists { r =>
          Reservation.Solo.AllAsSet.toSet[Reservation](r) || r == Reservation.Throwing
        })
    }

    implicit class RichReservationFilter(private val f: ReservationFilter) extends AnyVal {
      def &&(that: ReservationFilter): ReservationFilter = f andThen that
    }
  }

  /**
   * Gens used to create an initial `FullGameState`.
   */
  case class InitialGens(
    rulesGen: Gen[Rules] = arbitrary[Rules],
    startingPlayerGen: Gen[PlayerPosition] = arbitrary[PlayerPosition],
    totalScoresGen: Gen[TotalScores] = Gen.const(TotalScores(List.empty)),
    dealtCardsGen: DeckRule => Gen[TableMap[Seq[Card]]] = Dealer.simpleGen(_)
  ) {
    def withConstRules(rules: Rules): InitialGens = copy(rulesGen = Gen.const(rules))
    def withConstDealtCards(cards: TableMap[Seq[Card]]): InitialGens =
      copy(dealtCardsGen = _ => Gen.const(cards))
  }

  private def withShuffledActionsGen(
    fullGameState: FullGameState,
    calls: TableMap[? <: PlayerAction[? <: GameState]]
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
    acknowledgement: Acknowledgement[? <: GameState]
  ): Gen[Option[FullGameState]] = {
    for {
      state <- fullGameStateGen
      stateOpt <- withShuffledActionsGen(state, TableMap.fill(acknowledgement))
    } yield stateOpt
  }

  private def collectSomeState[State <: FullGameState: ClassTag](
    fullGameStateOptGen: Gen[Option[FullGameState]]
  ): Gen[State] =
    fullGameStateOptGen
      .suchThat {
        case Some(_: State) => true
        case _              => false
      }
      .map(_.get.asInstanceOf[State])

  private def validReservationGen(
    s: Negotiating.PlayerState,
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[PlayerAction.CallReservation] = {
    val all: Seq[Option[Reservation]] =
      reservationFilter(None +: s.reservationState.fold(_.map(Option.apply), _ => Seq.empty))
    Gen.oneOf(all).map(PlayerAction.CallReservation.apply)
  }

  private[game] def withSpecialHand[State <: FullGameState](
    handsGen: DeckRule => Gen[(PlayerPosition, TableMap[Seq[Card]])],
    stateGen: InitialGens => Gen[State],
    initialGensTemplate: InitialGens = InitialGens()
  ): Gen[(PlayerPosition, State)] = {
    for {
      rules <- initialGensTemplate.rulesGen
      (pos, hands) <- handsGen(rules.deckRule)
      gens = initialGensTemplate.withConstRules(rules).withConstDealtCards(hands)
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
      r <- gens.rulesGen
      sp <- gens.startingPlayerGen
      ts <- gens.totalScoresGen
      dc <- gens.dealtCardsGen(r.deckRule)
    } yield FullGameState.Negotiating.withDealtCards(sp, ts, dc)(r)
  }

  /**
   * Generates via `negotiatingGen` and then executes four valid `CallReservation` calls (one for each player).
   *
   * @note This should always generate `Some(state: FullGameState.NegotiationsResult)`. See `negotiationsResultGen`
   *       for a convenient variant that reflects that fact via its return type.
   */
  private[game] def negotiatingAfterFourValidReservationsGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[Option[FullGameState]] = {
    for {
      neg <- RuleConformingGens.negotiatingGen(gens)
      calls <- TableMapGens.flatMappedTableMapGen(
        neg.players,
        validReservationGen(_, reservationFilter)
      )
      stateOpt <- withShuffledActionsGen(neg, calls)
    } yield stateOpt
  }

  private[game] def negotiationsResultGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
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
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[Option[FullGameState]] = {
    acknowledgedGen(
      negotiationsResultGen(gens, reservationFilter),
      PlayerAction.AcknowledgeReservation
    )
  }

  private[game] def negotiationsResultFollowUpGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[FullGameState] = {
    collectSomeState[FullGameState](
      acknowledgedNegotiationsResultGen(gens, reservationFilter)
    )
  }

  /**
   * Generates a poverty as the "winning reservation" and makes all players acknowledge.
   *
   * @note This should always generate `Some(state: PovertyOnOffer)`.
   *       See `povertyOnOfferGen` for a convenient variant that reflects that fact via its
   *       return type.
   */
  private[game] val acknowledgedPovertyNegotiationsResultFollowUpGen = {
    acknowledgedNegotiationsResultGen(
      InitialGens(dealtCardsGen = Dealer.povertyGen(_).map { case (_, cards) => cards }),
      ReservationFilter.forcePoverty
    )
  }

  private[game] val povertyOnOfferGen: Gen[FullGameState.PovertyOnOffer] = {
    collectSomeState[FullGameState.PovertyOnOffer](acknowledgedPovertyNegotiationsResultFollowUpGen)
  }

  private[game] val povertyRefusedGen: Gen[FullGameState.PovertyRefused] = {
    @tailrec
    def keepRefusing(state: FullGameState): FullGameState = state match {
      case poo: FullGameState.PovertyOnOffer =>
        // we can assume that poo.handleAction is defined here (and otherwise, it might be best to fail fast)
        keepRefusing(
          poo.handleAction(poo.playerBeingOffered -> PlayerAction.PovertyReply(accepted = false))
        )
      case other =>
        other
    }
    collectSomeState[FullGameState.PovertyRefused] {
      povertyOnOfferGen.map(poo => Some(keepRefusing(poo)))
    }
  }

  /**
   * Generates `PovertyOnOffer` and then makes one of the offered players accept the poverty.
   *
   * @note This should always generate `Some(state: PovertyExchange)`.
   *       See `povertyExchangeGen` for a convenient variant that reflects that fact via its
   *       return type.
   */
  private[game] val povertyOnOfferAcceptedFollowUpGen: Gen[Option[FullGameState]] = {
    def acceptOrRefuse(gs: FullGameState, acceptingPlayer: PlayerPosition): Option[FullGameState] =
      gs match {
        case poo: FullGameState.PovertyOnOffer if poo.playerBeingOffered == acceptingPlayer =>
          poo.handleAction.lift(acceptingPlayer -> PlayerAction.PovertyReply(accepted = true))
        case poo: FullGameState.PovertyOnOffer =>
          poo.handleAction
            .lift(poo.playerBeingOffered -> PlayerAction.PovertyReply(false))
            .flatMap(acceptOrRefuse(_, acceptingPlayer))
        case x =>
          throw new MatchError(s"Expecting PovertyOnOffer, got $x")
      }

    for {
      onOffer <- povertyOnOfferGen
      acceptingPlayer <- Gen.oneOf(PlayerPosition.All.filterNot(_ == onOffer.poorPlayer))
    } yield acceptOrRefuse(onOffer, acceptingPlayer)
  }

  private[game] val povertyExchangeGen: Gen[FullGameState.PovertyExchange] = {
    collectSomeState[FullGameState.PovertyExchange](povertyOnOfferAcceptedFollowUpGen)
  }

  /**
   * Generates `PovertyExchange` and then makes the accepting player return sufficient cards.
   *
   * @note This should always generate `Some(state: Playing)`.
   *       See `playingAfterPovertyExchangeGen` for a convenient variant that reflects that fact via its
   *       return type.
   */
  private[game] val povertyExchangeFollowUpGen: Gen[Option[FullGameState]] = {
    for {
      exchange <- povertyExchangeGen
      acceptingPlayer = exchange.acceptingPlayer
      acceptingState = exchange.playerStates(acceptingPlayer)
      returned <- GenUtils.takeSomeUntil(acceptingState.hand)(
        _.size == exchange.rules.deckRule.cardsPerPlayer
      )
    } yield {
      val allSelected = returned.foldLeft[FullGameState](exchange) { (state, card) =>
        state.handleAction
          .lift(acceptingPlayer -> PlayerAction.PovertySelect(card))
          .getOrElse(state)
      }
      allSelected.handleAction.lift(
        exchange.acceptingPlayer -> PlayerAction.PovertyReturn
      )
    }
  }

  private[game] val playingAfterPovertyExchangeGen: Gen[FullGameState.Playing] = {
    collectSomeState[FullGameState.Playing](povertyExchangeFollowUpGen)
  }

  /**
   * Generates a valid initial `FullGameState.Playing`.
   */
  private[game] def playingGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[FullGameState.Playing] = {
    val includePoverty = {
      val somePov = Some(Reservation.Poverty)
      reservationFilter(Seq(somePov)).contains(somePov)
    }
    val withoutPoverty = {
      val filter: ReservationFilter = {
        import ReservationFilter.*
        reservationFilter && notPoverty && notThrowing
      }
      collectSomeState[FullGameState.Playing](
        acknowledgedNegotiationsResultGen(
          gens = gens,
          reservationFilter = filter
        )
      )
    }

    if (includePoverty)
      // realistically, poverty occurs less frequently, but we want it covered...
      Gen.frequency(8 -> withoutPoverty, 1 -> playingAfterPovertyExchangeGen)
    else
      withoutPoverty
  }

  /**
   * Generates via `playingGen` and then executes valid calls.
   */
  private def playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(
    deltaCards: Int = 1,
    gens: InitialGens,
    reservationFilter: ReservationFilter
  ): Gen[Option[FullGameState]] = {

    def play(playing: FullGameState.Playing, played: Int): Gen[Option[FullGameState]] = {

      def withAcknowledgements: Gen[(Option[FullGameState], Boolean)] = {
        val isLastTrick = playing.players.values.forall(_.hand.isEmpty)
        acknowledgedGen(Gen.const(playing), PlayerAction.AcknowledgeTrickResult)
          .map(_ -> isLastTrick)
      }

      def genNextState: Gen[(Option[FullGameState], Boolean, Boolean)] = {
        val regularActionsGen: Gen[(Option[FullGameState], Boolean, Boolean)] = {
          playing.finishedTrickOpt.fold {
            // trick is being played - one player must be allowed to play a card:
            val playable = playing.playerStates.toMap.filter { case (_, player) =>
              player.canPlay.nonEmpty
            }
            for {
              (pos, player) <- Gen.oneOf(playable)
              card <- Gen.oneOf(player.canPlay)
            } yield {
              (playing.handleAction.lift(pos -> PlayerAction.PlayCard(card)), false, true)
            }
          } { _ =>
            // trick is done: acknowledge for all players:
            withAcknowledgements.map { case (stateOpt, ifFinished) =>
              (stateOpt, ifFinished, false)
            }
          }
        }
        val minBids: Map[PlayerPosition, Bid] = playing.playerStates.collect {
          case (pos, state) if state.possibleBid.nonEmpty => pos -> state.possibleBid.get
        }.toMap

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

      if (played >= playing.rules.deckRule.cardsInPack + deltaCards) {
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
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[Option[FullGameState]] = {
    for {
      rules <- gens.rulesGen
      nCards <- Gen.choose(-rules.deckRule.cardsInPack, -1)
      updatedGens = gens.copy(rulesGen = Gen.const(rules))
      stateOpt <- playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(
        nCards,
        updatedGens,
        reservationFilter
      )
    } yield stateOpt
  }

  private[game] def playingMidGame(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[FullGameState.Playing] = {
    collectSomeState[FullGameState.Playing] {
      playingAfterLessThanAllCardsHaveBeenPlayed(gens, reservationFilter)
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
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[Option[FullGameState]] = {
    playingNCardsHaveBeenPlayedAndPossiblyAcknowledged(
      gens = gens,
      reservationFilter = reservationFilter
    )
  }

  private[game] def roundResultsGen(
    gens: InitialGens = InitialGens(),
    reservationFilter: ReservationFilter = ReservationFilter.neutral
  ): Gen[FullGameState.RoundResults] = {
    collectSomeState[FullGameState.RoundResults](
      playingAfterAllCardsHaveBeenPlayedAndAcknowledged(gens, reservationFilter)
    )
  }

  val fullGameStateGen: Gen[FullGameState] = Gen.frequency(
    1 -> negotiatingGen(),
    1 -> negotiationsResultFollowUpGen(),
    1 -> povertyOnOfferGen,
    1 -> povertyExchangeGen,
    1 -> povertyRefusedGen,
    1 -> playingGen(),
    5 -> playingMidGame(),
    1 -> roundResultsGen()
  )

  def genValidAction(fullGameState: FullGameState): Gen[(PlayerPosition, PlayerAction[GameState])] =
    import PlayerAction.*
    import FullGameState.*
    val actions: Seq[(PlayerPosition, PlayerAction[GameState])] =
      fullGameState match {
        case n: Negotiating =>
          n.players
            .collect { case (p, Negotiating.PlayerState(_, Left(reservations))) =>
              (p -> CallReservation(None)) +: reservations.map(r => p -> CallReservation(Some(r)))
            }
            .flatten
            .toSeq
        case n: NegotiationsResult =>
          n.missingAcks.map(_ -> AcknowledgeReservation).toSeq
        case p: PovertyOnOffer =>
          Seq(true, false).map(p.playerBeingOffered -> PovertyReply(_))
        case p: PovertyRefused =>
          p.missingAcks.map(_ -> AcknowledgePovertyRefused).toSeq
        case p: PovertyExchange =>
          val pos = p.acceptingPlayer
          val state = p.playerStates(pos)
          state.playerState.fold(Seq.empty[(PlayerPosition, PlayerAction[GameState])]) { ps =>
            val selectsOrReturn =
              if (ps.selected.size < state.sizeOfPoverty) ps.hand.map(PovertySelect.apply)
              else Seq(PovertyReturn)
            val deselects = ps.selected.map(PovertyDeselect.apply)
            (selectsOrReturn ++ deselects).map(pos -> _)
          }
        case p: Playing =>
          p.playerStates
            .collect { case (pos, GameState.Playing(_, _, _, _, Some(ps), _)) =>
              val bids =
                ps.possibleBid
                  .fold(Seq.empty[Bid])(b => Bid.All.filter(Bid.ordering.gteq(_, b)))
                  .map(PlaceBid.apply)
              val cardsOrAck =
                if (ps.needsAck) Seq(AcknowledgeTrickResult) else ps.canPlay.map(PlayCard.apply)
              (bids ++ cardsOrAck).map(pos -> _)
            }
            .flatten
            .toSeq
        case r: RoundResults =>
          r.missingAcks.map(_ -> AcknowledgeRoundResult).toSeq
      }
    Gen.oneOf(actions).suchThat(a => fullGameState.handleAction.isDefinedAt(a))
}

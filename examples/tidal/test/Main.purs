module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Cycle(..), Sample(..), TidalNote'(..), cycleP, cycleToSequence, unrest)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.StringParser (runParser)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Tests parser" do
          it "Works on simple imput" do
            runParser cycleP
              "hh:0" `shouldEqual` Right (SingleSample $ Just ClosedHH0)
            runParser cycleP
              "  hh:0" `shouldEqual` Right (SingleSample $ Just ClosedHH0)
            runParser cycleP
              "hh:0 snare:0" `shouldEqual` Right (Sequential (NonEmptyList ((SingleSample $ Just ClosedHH0) :| (SingleSample $ Just Snare0) : Nil)))
          it "Works on internal cycles" do
            runParser cycleP
              "[hh:0 snare:0] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList ((SingleSample $ Just ClosedHH0) :| (SingleSample $ Just Snare0) : Nil)) :| (SingleSample $ Just ClosedHH0) : Nil)))
          it "Works on branching cycles" do
            runParser cycleP
              "[hh:0 <snare:0 kick:0>] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList ((SingleSample $ Just ClosedHH0) :| (Branching (NonEmptyList ((SingleSample $ Just Snare0) :| (SingleSample $ Just Kick0) : Nil))) : Nil)) :| (SingleSample $ Just ClosedHH0) : Nil)))
          it "Works on simultaneous cycles" do
            runParser cycleP
              "[hh:0 <snare:0 kick:0>] , hh:0" `shouldEqual` Right (Simultaneous (NonEmptyList (Internal (NonEmptyList ((SingleSample $ Just ClosedHH0) :| (Branching (NonEmptyList ((SingleSample $ Just Snare0) :| (SingleSample $ Just Kick0) : Nil))) : Nil)) :| (SingleSample $ Just ClosedHH0) : Nil)))
          it "Works on branching cycles with internal cycle" do
            runParser cycleP
              "[hh:0 <[snare:0 snare:0] kick:0>] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList ((SingleSample $ Just ClosedHH0) :| (Branching (NonEmptyList ((Internal (NonEmptyList ((SingleSample $ Just Snare0) :| (SingleSample $ Just Snare0) : Nil))) :| (SingleSample $ Just Kick0) : Nil))) : Nil)) :| (SingleSample $ Just ClosedHH0) : Nil)))
        describe "Test cycle to sequence" do
          let run = map (cycleToSequence $ wrap 1.0) <<< runParser cycleP
          it "Works on simple imput" do
            run "hh:0" `shouldEqual` Right (pure (NonEmptyList (TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| Nil)))
            run "hh:0 snare:0" `shouldEqual` Right (pure (NonEmptyList (TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Just Snare0 } : Nil)))
          it "Works on internal cycles" do
            run "[hh:0 snare:0] hh:0" `shouldEqual` Right (pure (NonEmptyList (TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Just Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Just ClosedHH0 } : Nil)))
          it "Works on branching cycles" do
            run "[hh:0 <snare:0 kick:0>] hh:0" `shouldEqual` Right (NonEmptyList ((NonEmptyList (TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Just Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Just ClosedHH0 } : Nil)) :| (NonEmptyList (TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Just Kick0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Just ClosedHH0 } : Nil)) : Nil))
          it "Works on simultaneous cycles" do
            run "[hh:0 <snare:0 ~>] , snare:0" `shouldEqual` Right (NonEmptyList ((NonEmptyList (TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: Just Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Just Snare0 } : Nil)) :| (NonEmptyList (TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: Just ClosedHH0 } :| TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: Just Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Nothing } : Nil)) : Nil))
        describe "Test cycle to sequence without rests" do
          let run = map (unrest <<< cycleToSequence (wrap 1.0)) <<< runParser cycleP
          it "Works on simple imput" do
            run "hh:0" `shouldEqual` Right (pure ((TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : Nil)))
            run "hh:0 snare:0" `shouldEqual` Right (pure ((TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Snare0 } : Nil)))
          it "Works on internal cycles" do
            run "[hh:0 snare:0] hh:0" `shouldEqual` Right (pure ((TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: ClosedHH0 } : Nil)))
          it "Works on branching cycles" do
            run "[hh:0 <snare:0 kick:0>] hh:0" `shouldEqual` Right ((((TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: ClosedHH0 } : Nil)) : ((TidalNote { duration: 0.25, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 0.25, startsAt: 0.25, cycleLength: 1.0, sample: Kick0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: ClosedHH0 } : Nil)) : Nil))
          it "Works on simultaneous cycles" do
            run "[hh:0 <snare:0 ~>] , snare:0" `shouldEqual` Right ((((TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: Snare0 } : TidalNote { duration: 0.5, startsAt: 0.5, cycleLength: 1.0, sample: Snare0 } : Nil)) : ((TidalNote { duration: 0.5, startsAt: 0.0, cycleLength: 1.0, sample: ClosedHH0 } : TidalNote { duration: 1.0, startsAt: 0.0, cycleLength: 1.0, sample: Snare0 } : Nil)) : Nil))

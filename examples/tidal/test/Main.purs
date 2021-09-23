module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List.Types (List(..), NonEmptyList(..))
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Cycle(..), Sample(..), cycleP)
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
            runParser (cycleP unit)
              "hh:0" `shouldEqual` Right (Sequential (NonEmptyList (SingleSample HiHat0 :| Nil)))
            runParser (cycleP unit)
              "  hh:0" `shouldEqual` Right (Sequential (NonEmptyList (SingleSample HiHat0 :| Nil)))
            runParser (cycleP unit)
              "hh:0 snare:0" `shouldEqual` Right (Sequential (NonEmptyList (SingleSample HiHat0 :| SingleSample Snare0 : Nil)))
          it "Works on internal cycles" do
            runParser (cycleP unit)
              "[hh:0 snare:0] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList (SingleSample HiHat0 :| SingleSample Snare0 : Nil)) :| SingleSample HiHat0 : Nil)))
          it "Works on branching cycles" do
            runParser (cycleP unit)
              "[hh:0 <snare:0 kick:0>] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList (SingleSample HiHat0 :| (Branching (NonEmptyList (SingleSample Snare0 :| SingleSample Kick0 : Nil))) : Nil)) :| SingleSample HiHat0 : Nil)))
          it "Works on branching cycles with internal cycle" do
            runParser (cycleP unit)
              "[hh:0 <[snare:0 snare:0] kick:0>] hh:0" `shouldEqual` Right (Sequential (NonEmptyList (Internal (NonEmptyList (SingleSample HiHat0 :| (Branching (NonEmptyList ((Internal (NonEmptyList (SingleSample Snare0 :| SingleSample Snare0 : Nil))) :| SingleSample Kick0 : Nil))) : Nil)) :| SingleSample HiHat0 : Nil)))
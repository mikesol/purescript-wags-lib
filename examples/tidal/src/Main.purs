module Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodeUnits (skipSpaces, string, char)
import Text.Parsing.StringParser.Combinators (between, many, sepBy1)
import WAGS.Lib.Learn (play)
import WAGS.Lib.Learn.Pitch (middleC)

data Sample = HiHat0 | Kick0 | Rest | Snare0

derive instance genericSample :: Generic Sample _
derive instance eqSample :: Eq Sample
derive instance ordSample :: Ord Sample
instance showSample :: Show Sample where
  show x = genericShow x

data Cycle
  = Branching (NonEmptyList Cycle)
  | Simultaneous (NonEmptyList Cycle)
  | Sequential (NonEmptyList Cycle)
  | Internal (NonEmptyList Cycle)
  | SingleSample Sample

derive instance genericCycle :: Generic Cycle _
derive instance eqCycle :: Eq Cycle
derive instance ordCycle :: Ord Cycle
instance showCycle :: Show Cycle where
  show x = genericShow x

notes :: Array (String /\ Sample)
notes = [ "hh:0" /\ HiHat0, "kick:0" /\ Kick0, "snare:0" /\ Snare0, "~" /\ Rest ]

sampleP :: Parser Sample
sampleP = go $ L.fromFoldable notes
  where
  go (a : b) = (try (string (fst a)) $> snd a) <|> go b
  go Nil = fail "Could not find a note"

internalCycleP :: Parser Cycle
internalCycleP = Internal <$>
  between (skipSpaces *> char '[' *> skipSpaces) (skipSpaces *> char ']' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cycleP unit
    case cyc of
      Sequential l -> pure l
      x -> pure (pure x)

branchingCycleP :: Parser Cycle
branchingCycleP = Branching <$>
  between (skipSpaces *> char '<' *> skipSpaces) (skipSpaces *> char '>' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cycleP unit
    case cyc of
      Sequential l -> pure l
      x -> pure (pure x)

simultaneousCycleP :: Unit -> Parser Cycle
simultaneousCycleP _ = Simultaneous <$> do
  skipSpaces
  cy <- reducedP
  _ <- comma
  nel <- sepBy1 reducedP comma
  skipSpaces
  pure (pure cy <> nel)
  where
  comma = skipSpaces *> char ',' *> skipSpaces
  reducedP = try branchingCycleP
    <|> try sequentialCycleP
    <|> try singleSampleP
    <|> fail "Could not parse cycle"

joinSequential :: List Cycle -> List Cycle
joinSequential Nil = Nil
joinSequential (Sequential (NonEmptyList (a :| b)) : c) = (a : joinSequential b) <> joinSequential c
joinSequential (a : b) = a : joinSequential b

sequentialCycleP :: Parser Cycle
sequentialCycleP = Sequential <$> do
  skipSpaces
  leadsWith <- try internalCycleP <|> singleSampleP
  skipSpaces
  rest <- joinSequential <$> many (cycleP unit)
  pure (NonEmptyList (leadsWith :| rest))

singleSampleP :: Parser Cycle
singleSampleP = SingleSample <$> do
  skipSpaces
  sample <- sampleP
  skipSpaces
  pure sample

cycleP :: Unit -> Parser Cycle
cycleP _ = try branchingCycleP
  <|> try (simultaneousCycleP unit)
  <|> try sequentialCycleP
  <|> try singleSampleP
  <|> fail "Could not parse cycle"

main :: Effect Unit
main = play middleC

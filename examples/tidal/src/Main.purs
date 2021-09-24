module Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Data.Filterable (filter, filterMap)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty (sortBy)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodeUnits (skipSpaces, string, char)
import Text.Parsing.StringParser.Combinators (between, many, sepBy1)
import WAGS.Lib.Learn (play)
import WAGS.Lib.Learn.Pitch (middleC)
import WAGS.Lib.Score (CfRest)

newtype CycleLength = CycleLength Number

derive instance newtypeCycleLength :: Newtype CycleLength _
derive instance eqCycleLength :: Eq CycleLength
derive instance ordCycleLength :: Ord CycleLength

newtype TidalNote' sample = TidalNote
  { sample :: sample
  , startsAt :: Number
  , duration :: Number
  , cycleLength :: Number
  }

type TidalNote = TidalNote' (Maybe Sample)
type TidalNote_ = TidalNote' Sample

derive instance nwetypeTidalNote :: Newtype (TidalNote' sample) _
derive instance genericTidalNote :: Generic (TidalNote' sample) _
derive instance eqTidalNote :: Eq sample => Eq (TidalNote' sample)
derive instance ordTidalNote :: Ord sample => Ord (TidalNote' sample)
instance showTidalNote :: Show sample => Show (TidalNote' sample) where
  show x = genericShow x

data Sample = HiHat0 | Kick0 | Snare0

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
  | SingleSample (Maybe Sample)

derive instance genericCycle :: Generic Cycle _
derive instance eqCycle :: Eq Cycle
derive instance ordCycle :: Ord Cycle
instance showCycle :: Show Cycle where
  show x = genericShow x

notes :: Array (String /\ Maybe Sample)
notes = [ "hh:0" /\ Just HiHat0, "kick:0" /\ Just Kick0, "snare:0" /\ Just Snare0, "~" /\ Nothing ]

sampleP :: Parser (Maybe Sample)
sampleP = go $ L.fromFoldable notes
  where
  go (a : b) = (try (string (fst a)) $> snd a) <|> go b
  go Nil = fail "Could not find a note"

internalcyclePInternal :: Parser Cycle
internalcyclePInternal = Internal <$>
  between (skipSpaces *> char '[' *> skipSpaces) (skipSpaces *> char ']' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cyclePInternal unit
    case cyc of
      Sequential l -> pure l
      x -> pure (pure x)

branchingcyclePInternal :: Parser Cycle
branchingcyclePInternal = Branching <$>
  between (skipSpaces *> char '<' *> skipSpaces) (skipSpaces *> char '>' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cyclePInternal unit
    case cyc of
      Sequential l -> pure l
      x -> pure (pure x)

simultaneouscyclePInternal :: Unit -> Parser Cycle
simultaneouscyclePInternal _ = Simultaneous <$> do
  skipSpaces
  cy <- reducedP
  _ <- comma
  nel <- sepBy1 reducedP comma
  skipSpaces
  pure (pure cy <> nel)
  where
  comma = skipSpaces *> char ',' *> skipSpaces
  reducedP = try branchingcyclePInternal
    <|> try sequentialcyclePInternal
    <|> try singleSampleP
    <|> fail "Could not parse cycle"

joinSequential :: List Cycle -> List Cycle
joinSequential Nil = Nil
joinSequential (Sequential (NonEmptyList (a :| b)) : c) = (a : joinSequential b) <> joinSequential c
joinSequential (a : b) = a : joinSequential b

sequentialcyclePInternal :: Parser Cycle
sequentialcyclePInternal = Sequential <$> do
  skipSpaces
  leadsWith <- try internalcyclePInternal <|> singleSampleP
  skipSpaces
  rest <- joinSequential <$> many (cyclePInternal unit)
  pure (NonEmptyList (leadsWith :| rest))

singleSampleP :: Parser Cycle
singleSampleP = SingleSample <$> do
  skipSpaces
  sample <- sampleP
  skipSpaces
  pure sample

cyclePInternal :: Unit -> Parser Cycle
cyclePInternal _ = try branchingcyclePInternal
  <|> try (simultaneouscyclePInternal unit)
  <|> try sequentialcyclePInternal
  <|> try singleSampleP
  <|> fail "Could not parse cycle"

cycleP :: Parser Cycle
cycleP = go <$> cyclePInternal unit
  where
  go (Branching (NonEmptyList (a :| Nil))) = go a
  go (Simultaneous (NonEmptyList (a :| Nil))) = go a
  go (Sequential (NonEmptyList (a :| Nil))) = go a
  go (Internal (NonEmptyList (a :| Nil))) = go a
  go (Branching nel) = Branching (map go nel)
  go (Simultaneous nel) = Simultaneous (map go nel)
  go (Sequential nel) = Sequential (map go nel)
  go (Internal nel) = Internal (map go nel)
  go (SingleSample sample) = SingleSample sample

flatMap :: NonEmptyList (NonEmptyList (NonEmptyList TidalNote)) -> NonEmptyList (NonEmptyList TidalNote)
flatMap (NonEmptyList (a :| Nil)) = a
flatMap (NonEmptyList (a :| b : c)) = join $ a # map \a' -> flatMap (wrap (b :| c)) # map \b' -> a' <> b'

cycleToSequence :: CycleLength -> Cycle -> NonEmptyList (NonEmptyList TidalNote)
cycleToSequence (CycleLength cycleLength) = go { currentSubdivision: cycleLength, currentOffset: 0.0 }
  where
  go state (Branching nel) = join $ map (go state) nel
  go state (Simultaneous nel) = map (sortBy (compare `on` (unwrap >>> _.startsAt)))
    $ flatMap
    $ map (go state) nel
  go state (Sequential nel) = seq state nel
  go state (Internal nel) = seq state nel
  go state (SingleSample sample) =
    pure $ pure $ TidalNote
      { duration: state.currentSubdivision
      , startsAt: state.currentOffset
      , sample
      , cycleLength
      }
  seq state nel =
    let
      currentSubdivision = state.currentSubdivision / (toNumber (NEL.length nel))
    in
      flatMap $ mapWithIndex
        ( go
            <<< { currentSubdivision, currentOffset: _ }
            <<< mul currentSubdivision
            <<< toNumber
        )
        nel

unrest :: NonEmptyList (NonEmptyList TidalNote) -> List (List TidalNote_)
unrest = filter (not <<< eq Nil) <<< NEL.toList <<< map go
  where
  go =
    filterMap
      ( \(TidalNote { startsAt, duration, cycleLength, sample }) ->
          TidalNote <<< { startsAt, duration, cycleLength, sample: _ } <$> sample
      ) <<< NEL.toList

asScore :: NonEmptyList (NonEmptyList TidalNote_) -> CfRest { sample :: Sample, duration :: Number }
asScore l = go 0.0 0.0 flattened
  where
  ll = NEL.length l
  flattened = join $ mapWithIndex
    ( \i -> map
        ( \(TidalNote { sample, duration, startsAt, cycleLength }) -> TidalNote
            { sample
            , duration
            , startsAt: startsAt + cycleLength * toNumber i
            , cycleLength: cycleLength * toNumber ll
            }
        )
    )
    l
  go currentCount prevCycleEnded (NonEmptyList (TidalNote a :| b)) =
    let
      st = prevCycleEnded + a.startsAt
    in
      { startsAfter: st - currentCount, rest: { sample: a.sample, duration: a.duration } } :<
        \_ -> uncurry (go st) case b of
          Nil -> (prevCycleEnded + a.cycleLength) /\ flattened
          (c : d) -> prevCycleEnded /\ NonEmptyList (c :| d)

main :: Effect Unit
main = play middleC

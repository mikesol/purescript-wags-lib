module Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree ((:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
import Data.Either (Either(..), hush)
import Data.Filterable (compact, filter, filterMap)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Lens (_2, over, traversed)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty (sortBy)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D16)
import Effect (Effect)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (satisfy)
import Text.Parsing.StringParser.CodeUnits (skipSpaces, string, char)
import Text.Parsing.StringParser.Combinators (between, many, sepBy)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Create.Optionals (gain, playBuf, speaker, subgraph)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), makeScoredBufferPool)
import WAGS.Lib.Learn (buffers, play, usingc)
import WAGS.Lib.Score (CfNoteStream)
import WAGS.Math (calcSlope)
import WAGS.Run (SceneI(..))
import WAGS.Subgraph (SubSceneSig)
import WAGS.WebAPI (BrowserAudioBuffer)

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

data Sample = Kick0 | Kick1 | SideStick0 | Snare0 | Clap0 | SnareRoll0 | ClosedHH0 | Shaker0 | OpenHH0 | Tamb0 | Crash0 | Ride0

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
notes =
  over (traversed <<< _2) Just
    [ "kick:0" /\ Kick0
    , "kick:1" /\ Kick1
    , "kick" /\ Kick0
    , "ss:0" /\ SideStick0
    , "ss" /\ SideStick0
    , "snare:0" /\ Snare0
    , "snare" /\ Snare0
    , "clap:0" /\ Clap0
    , "clap" /\ Clap0
    , "roll:0" /\ SnareRoll0
    , "roll" /\ SnareRoll0
    , "hh:0" /\ ClosedHH0
    , "hh" /\ ClosedHH0
    , "shaker:0" /\ Shaker0
    , "shaker" /\ Shaker0
    , "ohh:0" /\ OpenHH0
    , "ohh" /\ OpenHH0
    , "tamb:0" /\ Tamb0
    , "tamb" /\ Tamb0
    , "crash:0" /\ Crash0
    , "crash" /\ Crash0
    , "ride:0" /\ Ride0
    , "ride" /\ Ride0
    ] <> [ "~" /\ Nothing ]

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
  sep <- sepBy ((fromCharArray <<< A.fromFoldable) <$> many (satisfy (not <<< eq ','))) (string ",")
  case sep of
    Nil -> fail "Lacks comma"
    (_ : Nil) -> fail "Lacks comma"
    (a : b : c) -> case traverse (runParser reducedP) (NonEmptyList (a :| b : c)) of
      Left e -> fail $ show e
      Right r -> pure r
  where
  reducedP = try branchingcyclePInternal
    <|> try sequentialcyclePInternal
    <|> try internalcyclePInternal
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
  go state (SingleSample sample) = pure $ pure $ TidalNote
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
            <<< add state.currentOffset
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

asScore :: NonEmptyList (NonEmptyList TidalNote_) -> CfNoteStream RBuf
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
      { startsAfter: st - currentCount, rest: { sampleF: sampleToBuffers a.sample, duration: a.duration } } :<
        \_ -> uncurry (go st) case b of
          Nil -> (prevCycleEnded + a.cycleLength) /\ flattened
          (c : d) -> prevCycleEnded /\ NonEmptyList (c :| d)

type NBuf
  = D16

type RBuf
  = { sampleF :: Instruments BrowserAudioBuffer -> BrowserAudioBuffer, duration :: Number }

type Acc
  = { buffers :: AScoredBufferPool Unit NBuf RBuf }

acc :: CfNoteStream RBuf -> Acc
acc cf =
  { buffers: makeScoredBufferPool
      { startsAt: 0.0
      , noteStream: \_ -> cf # map \{ startsAfter, rest } ->
          { startsAfter
          , rest:
              { rest: const rest
              , duration: const $ const $ const Just rest.duration
              }
          }
      }
  }

globalFF = 0.03 :: Number

type Instruments'' (a :: Type) (r :: Row Type)
  = (kick0 :: a, sideStick0 :: a, snare0 :: a, clap0 :: a, snareRoll0 :: a, kick1 :: a, closedHH0 :: a, shaker0 :: a, openHH0 :: a, tamb0 :: a, crash0 :: a, ride0 :: a | r)

type Instruments' (a :: Type)
  = Instruments'' a ()

type Instruments a
  = { | Instruments' a }

sampleToBuffers :: Sample -> Instruments BrowserAudioBuffer -> BrowserAudioBuffer
sampleToBuffers = case _ of
  Kick0 -> _.kick0
  Kick1 -> _.kick1
  SideStick0 -> _.sideStick0
  Snare0 -> _.snare0
  Clap0 -> _.clap0
  SnareRoll0 -> _.snareRoll0
  ClosedHH0 -> _.closedHH0
  Shaker0 -> _.shaker0
  OpenHH0 -> _.openHH0
  Tamb0 -> _.tamb0
  Crash0 -> _.crash0
  Ride0 -> _.ride0

tidal :: Number -> String -> Effect Unit
tidal dur =
  maybe (play "https://freesound.org/data/previews/350/350439_4557960-hq.mp3")
    ( \i -> play $ usingc
        ( buffers
            { kick0: "https://freesound.org/data/previews/171/171104_2394245-hq.mp3"
            , sideStick0: "https://freesound.org/data/previews/209/209890_3797507-hq.mp3"
            , snare0: "https://freesound.org/data/previews/495/495777_10741529-hq.mp3"
            , clap0: "https://freesound.org/data/previews/183/183102_2394245-hq.mp3"
            , snareRoll0: "https://freesound.org/data/previews/50/50710_179538-hq.mp3"
            , kick1: "https://freesound.org/data/previews/148/148634_2614600-hq.mp3"
            , closedHH0: "https://freesound.org/data/previews/269/269720_4965320-hq.mp3"
            , shaker0: "https://freesound.org/data/previews/432/432205_8738244-hq.mp3"
            , openHH0: "https://freesound.org/data/previews/416/416249_8218607-hq.mp3"
            , tamb0: "https://freesound.org/data/previews/207/207925_19852-hq.mp3"
            , crash0: "https://freesound.org/data/previews/528/528490_3797507-hq.mp3"
            , ride0: "https://freesound.org/data/previews/270/270138_1125482-hq.mp3"
            }
        )
        (acc i)
        \(SceneI { time: time', headroomInSeconds, world: { buffers } }) control ->
          let
            actualized = control.buffers { time: time', headroomInSeconds, input: unit }

            internal0 :: SubSceneSig "singleton" ()
              { buf :: Maybe (Buffy RBuf)
              , time :: Number
              }
            internal0 = unit # SG.loopUsingScene \{ time, buf } _ ->
              { control: unit
              , scene:
                  { singleton: case buf of
                      Just (Buffy { starting, startTime, rest: { sampleF, duration } }) ->
                        gain
                          ( ff globalFF $ pure $
                              if time > startTime + duration then calcSlope (startTime + duration) 1.0 (startTime + duration + 0.5) 0.0 time else 1.0
                          )
                          ( playBuf
                              { onOff:
                                  ff globalFF
                                    $
                                      if starting then
                                        ff (max 0.0 (startTime - time)) (pure OffOn)
                                      else
                                        pure On
                              }
                              (sampleF buffers)
                          )
                      Nothing -> gain 0.0 (playBuf { onOff: Off } buffers.kick1)
                  }
              }
          in
            { control: { buffers: unwrapCofree actualized }
            , scene: speaker
                { gn: gain 1.0
                    { sg: subgraph
                        (extract actualized)
                        (const $ const internal0)
                        (const $ { time: time', buf: _ })
                        {}
                    }

                }
            }
    )
    <<< map asScore
    <<< join
    <<< map
      ( NEL.fromList
          <<< compact
          <<< map NEL.fromList
          <<< (unrest <<< cycleToSequence (wrap dur))
      )
    <<< hush
    <<< runParser cycleP

main :: Effect Unit
main = tidal 1.0 "[hh:0 <kick:0 snare:0>] clap:0"
--main = tidal 2.0 "[hh hh hh] [clap roll clap <roll shaker>]"
--main = tidal 1.0 "kick kick clap ~"
--main = tidal 1.5 "clap [hh <ohh hh>] [roll <hh clap tamb>] , kick:0 kick:1"

module Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree ((:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Either (hush)
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
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D16)
import Effect (Effect)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodeUnits (skipSpaces, string, char)
import Text.Parsing.StringParser.Combinators (between, many, sepBy1)
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (speaker, gain, playBuf)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), makeScoredBufferPool)
import WAGS.Lib.Learn (buffers, play, usingc)
import WAGS.Lib.Score (CfNoteStream)
import WAGS.Math (calcSlope)
import WAGS.Run (SceneI(..))
import WAGS.Template (fromTemplate)
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
      { startsAfter: st - currentCount, rest: { sample: a.sample, duration: a.duration } } :<
        \_ -> uncurry (go st) case b of
          Nil -> (prevCycleEnded + a.cycleLength) /\ flattened
          (c : d) -> prevCycleEnded /\ NonEmptyList (c :| d)

{-
usingc
  :: forall trigger world scene graph control
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> control
  -> (SceneI { | trigger } { | world } EmptyAnalysers -> control -> { scene :: { | scene }, control :: control })
  -> FullSceneBuilder trigger world Unit
usingc triggerWorld control piece = FullSceneBuilder { triggerWorld, piece: loopUsingScene piece control }-}

type NBuf
  = D16

type RBuf
  = { sample :: Sample, duration :: Number }

type Acc
  = { buffers :: AScoredBufferPool NBuf RBuf }

acc :: CfNoteStream { sample :: Sample, duration :: Number } -> Acc
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
sampleToBuffers Kick0 b = b.kick0
sampleToBuffers Kick1 b = b.kick1
sampleToBuffers SideStick0 b = b.sideStick0
sampleToBuffers Snare0 b = b.snare0
sampleToBuffers Clap0 b = b.clap0
sampleToBuffers SnareRoll0 b = b.snareRoll0
sampleToBuffers ClosedHH0 b = b.closedHH0
sampleToBuffers Shaker0 b = b.shaker0
sampleToBuffers OpenHH0 b = b.openHH0
sampleToBuffers Tamb0 b = b.tamb0
sampleToBuffers Crash0 b = b.crash0
sampleToBuffers Ride0 b = b.ride0

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
        \(SceneI { time, headroomInSeconds, world: { buffers } }) control ->
          let
            actualized = control.buffers { time, headroomInSeconds }
          in
            { control: { buffers: unwrapCofree actualized }
            , scene: speaker
                ( gain (if time < 5.0 then time / 5.0 else 1.0)
                    -- todo: use ffi to speed up
                    ( fromTemplate (Proxy :: _ "instruments") (extract actualized) \_ -> case _ of
                        Just (Buffy { starting, startTime, rest: { sample, duration } }) ->
                          gain (ff globalFF $ pure $ if time > startTime + duration then calcSlope (startTime + duration) 1.0 (startTime + duration + 0.5) 0.0 time else 1.0)
                            ( playBuf
                                { onOff:
                                    ff globalFF
                                      $
                                        if starting then
                                          ff (max 0.0 (startTime - time)) (pure OffOn)
                                        else
                                          pure On
                                }
                                (sampleToBuffers sample buffers)
                            )
                        Nothing -> gain 0.0 (playBuf { onOff: Off } buffers.kick1)
                    )
                )
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
main = tidal 1.0 "[hh:0 <snare:0 kick:0>] clap:0"

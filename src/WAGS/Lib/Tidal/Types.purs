module WAGS.Lib.Tidal.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (hoistCofree)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Typelevel.Num (class Nat, class Succ, D0, D1, D8)
import Data.Variant.Either (either, Either)
import Data.Variant.Maybe (Maybe)
import Data.Vec as V
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Lib.BufferPool (AScoredBufferPool)
import WAGS.Lib.Score (CfNoteStream')
import WAGS.Tumult (Tumultuous)
import WAGS.WebAPI (BrowserAudioBuffer)

--
type IsFresh val
  = { isFresh :: Boolean, value :: val }

--
type ForwardBackwards
  = { forward :: BrowserAudioBuffer, backwards :: BrowserAudioBuffer }

newtype BufferUrl
  = BufferUrl String

derive instance newtypeBufferUrl :: Newtype BufferUrl _

derive instance eqBufferUrl :: Eq BufferUrl

derive instance ordBufferUrl :: Ord BufferUrl

instance showBufferUrl :: Show BufferUrl where
  show (BufferUrl s) = "BufferUrl <" <> s <> ">"

type SampleCache
  = Map Sample { url :: BufferUrl, buffer :: ForwardBackwards }

--- @@ ---
type RBuf event
  =
  { sampleFoT :: Either (UnsampledTimeIs event -> Sample) Sample
  , forward :: Boolean
  , rateFoT :: FoT event
  , bufferOffsetFoT :: FoT event
  , volumeFoT :: FoT event
  , cycleStartsAt :: Number
  , bigCycleDuration :: Number
  , littleCycleDuration :: Number
  , currentCycle :: Int
  , bigStartsAt :: Number
  , littleStartsAt :: Number
  , duration :: Number
  }

type CycleInfo
  =
  { cycleStartsAt :: Number
  , bigCycleDuration :: Number
  , littleCycleDuration :: Number
  , currentCycle :: Int
  , bigStartsAt :: Number
  , littleStartsAt :: Number
  , duration :: Number
  }

type TidalRes = { | EWF (Array CycleInfo) }

newtype NextCycle event
  = NextCycle
  { force :: Boolean
  , samples :: Array Sample
  , func ::
      { currentCount :: Number
      , prevCycleEnded :: Number
      , time :: Number
      , headroomInSeconds :: Number
      }
      -> CfNoteStream' (RBuf event) (Next event)
  }

instance semigroupNextCycle :: Semigroup (NextCycle event) where
  append (NextCycle a) next@(NextCycle b) = NextCycle
    { force: a.force || b.force
    , samples: Array.nub (a.samples <> b.samples)
    , func: map (hoistCofree (\y { time, headroomInSeconds } -> y { time, headroomInSeconds, input: { next } })) a.func
    }

derive instance newtypeNextCycle :: Newtype (NextCycle event) _

newtype Globals event
  = Globals
  { gain :: O'Past event
  , fx :: ClockTimeIs event -> Tumultuous D1 "output" (voice :: Unit)
  }

derive instance newtypeGlobals :: Newtype (Globals event) _

combineGlobals :: forall event. CycleDuration -> Globals event -> Globals event -> Globals event
combineGlobals (CycleDuration breakpoint) (Globals a) (Globals b) = Globals
  { gain:
      \ipt@
         ( TimeIsAndWas
             { timeIs: timeIs@(ClockTimeIs ti)
             , valWas
             , timeWas
             }
         ) ->
        if ti.adulteratedClockTime < breakpoint then a.gain ipt
        else b.gain
          ( TimeIsAndWas
              { timeIs: turnBackTime breakpoint timeIs
              , valWas
              , timeWas: map (turnBackTime breakpoint) timeWas
              }
          )
  , fx: \ipt@(ClockTimeIs timeIs) ->
      if timeIs.adulteratedClockTime < breakpoint then a.fx ipt
      else b.fx (turnBackTime breakpoint ipt)
  }

newtype Voice event
  = Voice { globals :: Globals event, next :: NextCycle event }

derive instance newtypeVoice :: Newtype (Voice event) _

combineVoices :: forall event. CycleDuration -> Voice event -> Voice event -> Voice event
combineVoices breakpoint (Voice a) (Voice b) = Voice
  { globals: combineGlobals breakpoint a.globals b.globals
  , next: a.next <> b.next
  }

type EWF' (v :: Type) r
  = (earth :: v, wind :: v, fire :: v | r)

type EWF (v :: Type)
  = EWF' v ()

type AH' (v :: Type) r
  = (air :: v, heart :: v | r)

type AH (v :: Type)
  = AH' v ()

newtype TheFuture event
  = TheFuture
  {
  | EWF' (Voice event)
      ( AH' (Maybe (DroneNote event))
          ( sounds :: Object BufferUrl
          , title :: String
          , preload :: Array Sample
          , cycleDuration :: CycleDuration
          )
      )
  }

instance semigroupTheFuture :: Semigroup (TheFuture event) where
  append (TheFuture a) (TheFuture b) = TheFuture
    { earth: combineVoices a.cycleDuration a.earth b.earth
    , wind: combineVoices a.cycleDuration a.wind b.wind
    , fire: combineVoices a.cycleDuration a.fire b.fire
    , air: a.air <|> b.air
    , heart: a.heart <|> b.heart
    , sounds: Object.union a.sounds b.sounds
    , title: a.title <> " <> " <> b.title
    , preload: a.preload <> b.preload
    , cycleDuration: a.cycleDuration + b.cycleDuration
    }

derive instance newtypeTheFuture :: Newtype (TheFuture event) _

--- @@ ---
newtype CycleDuration
  = CycleDuration Number

derive instance newtypeCycleDuration :: Newtype CycleDuration _

derive instance eqCycleDuration :: Eq CycleDuration

derive instance ordCycleDuration :: Ord CycleDuration

derive newtype instance semiringCycleDuration :: Semiring CycleDuration

newtype NoteInTime note
  = NoteInTime
  { note :: note
  , startsAt :: Number
  , duration :: Number
  , cycleDuration :: Number
  , tag :: Maybe String
  }

derive instance newtypeNoteInTime :: Newtype (NoteInTime note) _

derive instance genericNoteInTime :: Generic (NoteInTime note) _

derive instance eqNoteInTime :: Eq note => Eq (NoteInTime note)

derive instance ordNoteInTime :: Ord note => Ord (NoteInTime note)

instance showNoteInTime :: Show note => Show (NoteInTime note) where
  show xx = genericShow xx

derive instance functorNoteInTime :: Functor NoteInTime

newtype NoteInFlattenedTime note
  = NoteInFlattenedTime
  { note :: note
  , bigStartsAt :: Number
  , littleStartsAt :: Number
  , currentCycle :: Int
  , positionInCycle :: Int
  , elementsInCycle :: Int
  , nCycles :: Int
  , duration :: Number
  , bigCycleDuration :: Number
  , littleCycleDuration :: Number
  , tag :: Maybe String
  }

derive instance newtypeNoteInFlattenedTime :: Newtype (NoteInFlattenedTime note) _

derive instance genericNoteInFlattenedTime :: Generic (NoteInFlattenedTime note) _

derive instance eqNoteInFlattenedTime :: Eq note => Eq (NoteInFlattenedTime note)

derive instance ordNoteInFlattenedTime :: Ord note => Ord (NoteInFlattenedTime note)

instance showNoteInFlattenedTime :: Show note => Show (NoteInFlattenedTime note) where
  show xx = genericShow xx

derive instance functorNoteInFlattenedTime :: Functor NoteInFlattenedTime

--
type AfterMatter
  = { asInternal :: Maybe (NonEmptyArray Unit) }

--
type Tag
  = { tag :: Maybe String }

--
type NBuf
  = D8

type Next event
  = { next :: NextCycle event }

type Acc event
  =
  { buffers :: { | EWF (AScoredBufferPool (Next event) NBuf (RBuf event)) }
  , justInCaseTheLastEvent :: IsFresh event
  }

---
class
  Nat n <=
  HomogenousToVec (rl :: RL.RowList Type) (r :: Row Type) (n :: Type) (a :: Type)
  | rl r -> n a where
  h2v' :: forall proxy. proxy rl -> { | r } -> V.Vec n a

instance h2vNil :: HomogenousToVec RL.Nil r D0 a where
  h2v' _ _ = V.empty

instance h2vCons ::
  ( IsSymbol key
  , Row.Lacks key r'
  , Row.Cons key a r' r
  , HomogenousToVec rest r n' a
  , Succ n' n
  ) =>
  HomogenousToVec (RL.Cons key a rest) r n a where
  h2v' _ r = V.cons (Record.get (Proxy :: _ key) r) (h2v' (Proxy :: _ rest) r)

----
newtype DroneNote event
  = DroneNote
  { sample :: Sample
  , forward :: Boolean
  , rateFoT :: O'Past event
  , loopStartFoT :: O'Past event
  , loopEndFoT :: O'Past event
  , volumeFoT :: O'Past event
  , tumultFoT :: ClockTimeIs event -> Tumultuous D1 "output" (voice :: Unit)
  }

derive instance newtypeDroneNote :: Newtype (DroneNote event) _

derive instance genericDroneNote :: Generic (DroneNote event) _

instance eqDroneNote :: Eq (DroneNote event) where
  eq = eq `on` (unwrap >>> _.sample)

instance ordDroneNote :: Ord (DroneNote event) where
  compare = compare `on` (unwrap >>> _.sample)

instance showDroneNote :: Show (DroneNote event) where
  show (DroneNote { sample }) = "DroneNote <" <> show sample <> ">"

----
newtype Note event
  = Note
  { sampleFoT :: Either (UnsampledTimeIs event -> Sample) Sample
  , forward :: Boolean
  , rateFoT :: FoT event
  , bufferOffsetFoT :: FoT event
  , volumeFoT :: FoT event
  }

unlockSample :: forall event. event -> UnsampledTimeIs event
unlockSample event =
  UnsampledTimeIs
    { clockTime: 0.0
    , event: { isFresh: true, value: event }
    , bigCycleTime: 0.0
    , littleCycleTime: 0.0
    , normalizedClockTime: 0.0
    , normalizedBigCycleTime: 0.0
    , normalizedLittleCycleTime: 0.0
    , littleCycleDuration: 0.0
    , bigCycleDuration: 0.0
    , entropy: 0.0
    , initialEntropy: 0.0
    }

derive instance newtypeNote :: Newtype (Note event) _

derive instance genericNote :: Generic (Note event) _

instance eqNote :: Monoid event => Eq (Note event) where
  eq = eq `on` (unwrap >>> _.sampleFoT >>> either ((#) (unlockSample mempty)) identity)

instance ordNote :: Monoid event => Ord (Note event) where
  compare = compare `on` (unwrap >>> _.sampleFoT >>> (either ((#) (unlockSample mempty)) identity))

instance showNote :: Monoid event => Show (Note event) where
  show (Note { sampleFoT }) = "Note <" <> show (either ((#) (unlockSample mempty)) identity sampleFoT) <> ">"

----------------------------------
newtype Sample
  = Sample String

derive instance sampleNewtype :: Newtype Sample _

derive instance sampleEq :: Eq Sample

derive instance sampleOrd :: Ord Sample

instance sampleShow :: Show Sample where
  show (Sample i) = "Sample <" <> show i <> ">"

type ClockTimeIs' event =
  { clockTime :: Number
  , adulteratedClockTime :: Number
  , event :: IsFresh event
  , entropy :: Number
  }

newtype ClockTimeIs event = ClockTimeIs (ClockTimeIs' event)

turnBackTime
  :: forall event
   . Number
  -> ClockTimeIs event
  -> ClockTimeIs event
turnBackTime
  n
  ( ClockTimeIs
      { clockTime
      , adulteratedClockTime
      , event
      , entropy
      }
  ) =
  ClockTimeIs
    { clockTime
    , adulteratedClockTime: adulteratedClockTime - n
    , event
    , entropy
    }

derive instance newtypeClockTimeIs :: Newtype (ClockTimeIs event) _

newtype UnsampledTimeIs event
  = UnsampledTimeIs
  { event :: IsFresh event
  , clockTime :: Number
  , bigCycleTime :: Number
  , littleCycleTime :: Number
  , normalizedClockTime :: Number
  , normalizedBigCycleTime :: Number
  , normalizedLittleCycleTime :: Number
  , littleCycleDuration :: Number
  , bigCycleDuration :: Number
  , entropy :: Number
  , initialEntropy :: Number
  }

derive instance newtypeUnsampledTimeIs :: Newtype (UnsampledTimeIs event) _

type TimeIs' event =
  { event :: IsFresh event
  , clockTime :: Number
  , sampleTime :: Number
  , bigCycleTime :: Number
  , littleCycleTime :: Number
  , normalizedClockTime :: Number
  , normalizedSampleTime :: Number
  , normalizedBigCycleTime :: Number
  , normalizedLittleCycleTime :: Number
  , littleCycleDuration :: Number
  , bigCycleDuration :: Number
  , bufferDuration :: Number
  , entropy :: Number
  , initialEntropy :: Number
  }

newtype TimeIs event = TimeIs (TimeIs' event)

derive instance newtypeTimeIs :: Newtype (TimeIs event) _

newtype TimeIsAndWas time
  = TimeIsAndWas
  { timeIs :: time
  , valWas :: Maybe Number
  , timeWas :: Maybe time
  }

derive instance newtypeTimeIsAndWas :: Newtype (TimeIsAndWas time) _

type O'Clock event
  = ClockTimeIs event -> Number

type O'Past event
  = TimeIsAndWas (ClockTimeIs event) -> Number

type FoT event
  = TimeIs event -> Number

type FoP event
  = TimeIsAndWas (TimeIs event) -> Number

type Samples a
  = Object a

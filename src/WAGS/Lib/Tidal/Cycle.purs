module WAGS.Lib.Tidal.Cycle where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (class Foldable, foldMapDefaultR, foldl, foldr, intercalate)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (floor)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Variant (Variant, match, inj)
import Data.Variant.Either (either, right)
import Data.Variant.Maybe (Maybe, just, nothing, maybe)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Tidal.FX (goodbye, hello, fx)
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.Types (DroneNote, Note(..), Sample, unlockSample, emptyCtrl)

type CycleEnv = { weight :: Number, tag :: Maybe String }

type Cycles a =
  { cycles :: NEA.NonEmptyArray (Cycle a)
  , env :: CycleEnv
  }

type Singleton' a = { val :: a, env :: CycleEnv }

branching :: Cycles ~> Cycle
branching = Cycle <<< inj (Proxy :: _ "branching")

simultaneous :: Cycles ~> Cycle
simultaneous = Cycle <<< inj (Proxy :: _ "simultaneous")

internal :: Cycles ~> Cycle
internal = Cycle <<< inj (Proxy :: _ "internal")

singleton :: Singleton' ~> Cycle
singleton = Cycle <<< inj (Proxy :: _ "singleton")

derive instance newtypeCycle :: Newtype (Cycle a) _
newtype Cycle a
  = Cycle
  ( Variant
      ( branching ::
          { cycles :: NEA.NonEmptyArray (Cycle a)
          , env :: CycleEnv
          }
      , simultaneous ::
          { cycles :: NEA.NonEmptyArray (Cycle a)
          , env :: CycleEnv
          }
      , internal ::
          { cycles :: NEA.NonEmptyArray (Cycle a)
          , env :: CycleEnv
          }
      , singleton :: { val :: a, env :: CycleEnv }
      )
  )

swapEnv :: CycleEnv -> Cycle ~> Cycle
swapEnv env = unwrap >>> match
  { branching: branching <<< { cycles: _, env } <<< _.cycles
  , simultaneous: simultaneous <<< { cycles: _, env } <<< _.cycles
  , internal: internal <<< { cycles: _, env } <<< _.cycles
  , singleton: singleton <<< { val: _, env } <<< _.val
  }

derive instance genericCycle :: Generic (Cycle a) _
derive instance eqCycle :: Eq a => Eq (Cycle a)
derive instance ordCycle :: Ord a => Ord (Cycle a)
instance showCycle :: Show a => Show (Cycle a) where
  show xx = genericShow xx

instance functorCycle :: Functor Cycle where
  map ff = unwrap >>> match
    { branching: \{ env, cycles } ->
        branching $ { env, cycles: (map <<< map) ff cycles }
    , simultaneous: \{ env, cycles } ->
        simultaneous { env, cycles: (map <<< map) ff cycles }
    , internal: \{ env, cycles } ->
        internal { env, cycles: (map <<< map) ff cycles }
    , singleton: \{ env, val } ->
        singleton { env, val: ff val }
    }

instance applyCycle :: Apply Cycle where
  apply = ap

instance applicativeCycle :: Applicative Cycle where
  pure = singleton <<<
    { env: emptyEnv
    , val: _
    }

instance bindCycle :: Bind Cycle where
  bind (Cycle ma) faMb = ma # match
    { branching: \{ env, cycles } ->
        branching $ { env, cycles: map (flip (>>=) faMb) cycles }
    , simultaneous: \{ env, cycles } ->
        simultaneous { env, cycles: map (flip (>>=) faMb) cycles }
    , internal: \{ env, cycles } ->
        internal { env, cycles: map (flip (>>=) faMb) cycles }
    , singleton: \{ env, val } ->
        swapEnv env $ faMb val
    }

instance monadCycle :: Monad Cycle

instance functorWithIndexCycle :: FunctorWithIndex Int Cycle where
  mapWithIndex fff vvv = (go 0 vvv).val
    where
    go' ff ii env cycles = let folded = foldl (\axc cyc -> axc <> (pure (go (NEA.last axc).i cyc))) (pure (go ii (NEA.head cycles))) (NEA.tail cycles) in { i: _.i $ NEA.last folded, val: ff { env, cycles: map _.val folded } }
    go ii = unwrap >>> match
      { branching: \{ env, cycles } -> go' branching ii env cycles
      , simultaneous: \{ env, cycles } -> go' simultaneous ii env cycles
      , internal: \{ env, cycles } -> go' internal ii env cycles
      , singleton: \{ env, val } -> { i: ii + 1, val: singleton { env, val: fff ii val } }
      }

instance foldableCycle :: Foldable Cycle where
  foldl fba aa fbb = foldl fba aa (flattenCycle fbb)
  foldr fab aa fbb = foldr fab aa (flattenCycle fbb)
  foldMap = foldMapDefaultR

instance traversableCycle :: Traversable Cycle where
  traverse ff = unwrap >>> match
    { branching: \{ env, cycles } ->
        branching <<< { env, cycles: _ } <$> (traverse (traverse ff) cycles)
    , simultaneous: \{ env, cycles } ->
        simultaneous <<< { env, cycles: _ } <$> (traverse (traverse ff) cycles)
    , internal: \{ env, cycles } ->
        internal <<< { env, cycles: _ } <$> (traverse (traverse ff) cycles)
    , singleton: \{ env, val } ->
        singleton <<< { env, val: _ } <$> ff val
    }
  sequence = sequenceDefault

flattenCycle :: Cycle ~> NEA.NonEmptyArray
flattenCycle = unwrap >>> match
  { branching: \{ cycles } -> join $ map flattenCycle cycles
  , simultaneous: \{ cycles } -> join $ map flattenCycle cycles
  , internal: \{ cycles } -> join $ map flattenCycle cycles
  , singleton: \{ val } -> pure val
  }

flatten :: Cycle ~> Cycle
flatten = internal 
  <<< { env: emptyEnv, cycles: _ }
  <<< go
  where
  go = unwrap >>> match
    { branching: \{ cycles } -> join $ map go cycles
    , simultaneous: \{ cycles } -> join $ map go cycles
    , internal: \{ cycles } -> join $ map go cycles
    , singleton:  NEA.singleton <<< singleton
    }

firstCycle :: forall a. Cycle a -> a
firstCycle = go
  where
  go' aa = go (NEA.head aa)
  go = unwrap >>> match
    { branching: \{ cycles } -> go' cycles
    , simultaneous: \{ cycles } -> go' cycles
    , internal: \{ cycles } -> go' cycles
    , singleton: \{ val } -> val
    }

lastCycle :: forall a. Cycle a -> a
lastCycle = go
  where
  go' aa = go (NEA.last aa)
  go = unwrap >>> match
    { branching: \{ cycles } -> go' cycles
    , simultaneous: \{ cycles } -> go' cycles
    , internal: \{ cycles } -> go' cycles
    , singleton: \{ val } -> val
    }

c2d :: forall event. Cycle (Maybe (Note event)) -> Maybe (DroneNote event)
c2d = firstCycle >>> maybe nothing just >>> map S.note2drone

reverse :: Cycle ~> Cycle
reverse l = go l
  where
  go' fff env cycles = fff { env, cycles: NEA.reverse (map reverse cycles) }
  go = unwrap >>> match
    { branching: \{ env, cycles } -> go' branching env cycles
    , simultaneous: \{ env, cycles } -> go' simultaneous env cycles
    , internal: \{ env, cycles } -> go' internal env cycles
    , singleton: \snnn -> singleton snnn
    }

-- can't be point free due to let
cycleLength :: forall a. Cycle a -> Int
cycleLength (Cycle incyc) =
  let
    cycleLength' = foldr (+) 0 <<< map cycleLength <<< _.cycles
  in
    incyc # match
      { branching: cycleLength'
      , simultaneous: cycleLength'
      , internal: cycleLength'
      , singleton: const 1
      }

cycleToString :: forall event. event -> Cycle (Maybe (Note event)) -> String
cycleToString event = go
  where
  ws env = if env.weight >= 2.0 then "*" <> show (floor env.weight) else ""
  tg env = maybe "" (append ";") env.tag
  go = unwrap >>> match
    { branching: \{ env, cycles } -> "<" <> intercalate " " (map go cycles) <> ">" <> ws env <> tg env
    , simultaneous: \{ env, cycles } -> (intercalate " , " (map go cycles)) <> ws env <> tg env
    , internal: \{ env, cycles } -> "[" <> intercalate " " (map go cycles) <> "]" <> ws env <> tg env
    , singleton: \{ env, val } ->
        ( maybe "~"
            ( S.sampleToString
                <<< either ((#) (unlockSample event emptyCtrl)) identity
                <<< _.sampleFoT
                <<< unwrap
            )
            val
        ) <> ws env <> tg env
    }

noteFromSample :: forall event. Sample -> Note event
noteFromSample = Note
  <<<
    { sampleFoT: _
    , rateFoT: const 1.0
    , volumeFoT: const 1.0
    , tumultFoT: const $ fx $ goodbye hello
    , bufferOffsetFoT: const 0.0
    , forwardFoT: const true
    }
  <<< right

noteFromSample_ :: Sample -> Note Unit
noteFromSample_ = noteFromSample

cycleFromSample' :: forall event. Number -> Sample -> Cycle (Maybe (Note event))
cycleFromSample' weight sample = singleton
  { env: { weight, tag: nothing }
  , val: just (noteFromSample sample)
  }

cycleFromSample :: forall event. Sample -> Cycle (Maybe (Note event))
cycleFromSample = cycleFromSample' 1.0

cycleFromSample'_ :: Number -> Sample -> Cycle (Maybe (Note Unit))
cycleFromSample'_ = cycleFromSample'

cycleFromSample_ :: Sample -> Cycle (Maybe (Note Unit))
cycleFromSample_ = cycleFromSample

intentionalSilenceForInternalUseOnly_ :: forall event. Cycle (Maybe (Note event))
intentionalSilenceForInternalUseOnly_ = cycleFromSample S.intentionalSilenceForInternalUseOnly__Sample

emptyEnv = { weight: 1.0, tag: nothing  } :: CycleEnv

r :: forall event. Cycle (Maybe (Note event))
r = singleton { val: nothing, env: emptyEnv }

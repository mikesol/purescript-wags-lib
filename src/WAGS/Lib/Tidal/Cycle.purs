module WAGS.Lib.Tidal.Cycle where

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultR, foldl, foldr, intercalate)
import Data.Newtype (class Newtype, unwrap)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (floor)
import Data.Variant (Variant, match, inj)
import Data.Array.NonEmpty as NEA
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Variant.Maybe (Maybe, just, nothing, maybe)
import Data.Variant.Either (either, right)
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.FX (goodbye, hello, fx)
import WAGS.Lib.Tidal.Types (DroneNote, Note(..), Sample, unlockSample, emptyCtrl)
import Type.Proxy (Proxy(..))

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
    { env: { weight: 1.0, tag: nothing }
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

r :: forall event. Cycle (Maybe (Note event))
r = singleton { val: nothing, env: { weight: 1.0, tag: nothing } }

kicklinn :: forall event. Cycle (Maybe (Note event))
kicklinn = cycleFromSample S.kicklinn_0__Sample

kicklinn_0 :: forall event. Cycle (Maybe (Note event))
kicklinn_0 = cycleFromSample S.kicklinn_0__Sample

msg :: forall event. Cycle (Maybe (Note event))
msg = cycleFromSample S.msg_0__Sample

msg_0 :: forall event. Cycle (Maybe (Note event))
msg_0 = cycleFromSample S.msg_0__Sample

msg_1 :: forall event. Cycle (Maybe (Note event))
msg_1 = cycleFromSample S.msg_1__Sample

msg_2 :: forall event. Cycle (Maybe (Note event))
msg_2 = cycleFromSample S.msg_2__Sample

msg_3 :: forall event. Cycle (Maybe (Note event))
msg_3 = cycleFromSample S.msg_3__Sample

msg_4 :: forall event. Cycle (Maybe (Note event))
msg_4 = cycleFromSample S.msg_4__Sample

msg_5 :: forall event. Cycle (Maybe (Note event))
msg_5 = cycleFromSample S.msg_5__Sample

msg_6 :: forall event. Cycle (Maybe (Note event))
msg_6 = cycleFromSample S.msg_6__Sample

msg_7 :: forall event. Cycle (Maybe (Note event))
msg_7 = cycleFromSample S.msg_7__Sample

msg_8 :: forall event. Cycle (Maybe (Note event))
msg_8 = cycleFromSample S.msg_8__Sample

gabbalouder :: forall event. Cycle (Maybe (Note event))
gabbalouder = cycleFromSample S.gabbalouder_0__Sample

gabbalouder_0 :: forall event. Cycle (Maybe (Note event))
gabbalouder_0 = cycleFromSample S.gabbalouder_0__Sample

gabbalouder_1 :: forall event. Cycle (Maybe (Note event))
gabbalouder_1 = cycleFromSample S.gabbalouder_1__Sample

gabbalouder_2 :: forall event. Cycle (Maybe (Note event))
gabbalouder_2 = cycleFromSample S.gabbalouder_2__Sample

gabbalouder_3 :: forall event. Cycle (Maybe (Note event))
gabbalouder_3 = cycleFromSample S.gabbalouder_3__Sample

kurt :: forall event. Cycle (Maybe (Note event))
kurt = cycleFromSample S.kurt_0__Sample

kurt_0 :: forall event. Cycle (Maybe (Note event))
kurt_0 = cycleFromSample S.kurt_0__Sample

kurt_1 :: forall event. Cycle (Maybe (Note event))
kurt_1 = cycleFromSample S.kurt_1__Sample

kurt_2 :: forall event. Cycle (Maybe (Note event))
kurt_2 = cycleFromSample S.kurt_2__Sample

kurt_3 :: forall event. Cycle (Maybe (Note event))
kurt_3 = cycleFromSample S.kurt_3__Sample

kurt_4 :: forall event. Cycle (Maybe (Note event))
kurt_4 = cycleFromSample S.kurt_4__Sample

kurt_5 :: forall event. Cycle (Maybe (Note event))
kurt_5 = cycleFromSample S.kurt_5__Sample

kurt_6 :: forall event. Cycle (Maybe (Note event))
kurt_6 = cycleFromSample S.kurt_6__Sample

bassdm :: forall event. Cycle (Maybe (Note event))
bassdm = cycleFromSample S.bassdm_0__Sample

bassdm_0 :: forall event. Cycle (Maybe (Note event))
bassdm_0 = cycleFromSample S.bassdm_0__Sample

bassdm_1 :: forall event. Cycle (Maybe (Note event))
bassdm_1 = cycleFromSample S.bassdm_1__Sample

bassdm_2 :: forall event. Cycle (Maybe (Note event))
bassdm_2 = cycleFromSample S.bassdm_2__Sample

bassdm_3 :: forall event. Cycle (Maybe (Note event))
bassdm_3 = cycleFromSample S.bassdm_3__Sample

bassdm_4 :: forall event. Cycle (Maybe (Note event))
bassdm_4 = cycleFromSample S.bassdm_4__Sample

bassdm_5 :: forall event. Cycle (Maybe (Note event))
bassdm_5 = cycleFromSample S.bassdm_5__Sample

bassdm_6 :: forall event. Cycle (Maybe (Note event))
bassdm_6 = cycleFromSample S.bassdm_6__Sample

bassdm_7 :: forall event. Cycle (Maybe (Note event))
bassdm_7 = cycleFromSample S.bassdm_7__Sample

bassdm_8 :: forall event. Cycle (Maybe (Note event))
bassdm_8 = cycleFromSample S.bassdm_8__Sample

bassdm_9 :: forall event. Cycle (Maybe (Note event))
bassdm_9 = cycleFromSample S.bassdm_9__Sample

bassdm_10 :: forall event. Cycle (Maybe (Note event))
bassdm_10 = cycleFromSample S.bassdm_10__Sample

bassdm_11 :: forall event. Cycle (Maybe (Note event))
bassdm_11 = cycleFromSample S.bassdm_11__Sample

bassdm_12 :: forall event. Cycle (Maybe (Note event))
bassdm_12 = cycleFromSample S.bassdm_12__Sample

bassdm_13 :: forall event. Cycle (Maybe (Note event))
bassdm_13 = cycleFromSample S.bassdm_13__Sample

bassdm_14 :: forall event. Cycle (Maybe (Note event))
bassdm_14 = cycleFromSample S.bassdm_14__Sample

bassdm_15 :: forall event. Cycle (Maybe (Note event))
bassdm_15 = cycleFromSample S.bassdm_15__Sample

bassdm_16 :: forall event. Cycle (Maybe (Note event))
bassdm_16 = cycleFromSample S.bassdm_16__Sample

bassdm_17 :: forall event. Cycle (Maybe (Note event))
bassdm_17 = cycleFromSample S.bassdm_17__Sample

bassdm_18 :: forall event. Cycle (Maybe (Note event))
bassdm_18 = cycleFromSample S.bassdm_18__Sample

bassdm_19 :: forall event. Cycle (Maybe (Note event))
bassdm_19 = cycleFromSample S.bassdm_19__Sample

bassdm_20 :: forall event. Cycle (Maybe (Note event))
bassdm_20 = cycleFromSample S.bassdm_20__Sample

bassdm_21 :: forall event. Cycle (Maybe (Note event))
bassdm_21 = cycleFromSample S.bassdm_21__Sample

bassdm_22 :: forall event. Cycle (Maybe (Note event))
bassdm_22 = cycleFromSample S.bassdm_22__Sample

bassdm_23 :: forall event. Cycle (Maybe (Note event))
bassdm_23 = cycleFromSample S.bassdm_23__Sample

tabla2 :: forall event. Cycle (Maybe (Note event))
tabla2 = cycleFromSample S.tabla2_0__Sample

tabla2_0 :: forall event. Cycle (Maybe (Note event))
tabla2_0 = cycleFromSample S.tabla2_0__Sample

tabla2_1 :: forall event. Cycle (Maybe (Note event))
tabla2_1 = cycleFromSample S.tabla2_1__Sample

tabla2_2 :: forall event. Cycle (Maybe (Note event))
tabla2_2 = cycleFromSample S.tabla2_2__Sample

tabla2_3 :: forall event. Cycle (Maybe (Note event))
tabla2_3 = cycleFromSample S.tabla2_3__Sample

tabla2_4 :: forall event. Cycle (Maybe (Note event))
tabla2_4 = cycleFromSample S.tabla2_4__Sample

tabla2_5 :: forall event. Cycle (Maybe (Note event))
tabla2_5 = cycleFromSample S.tabla2_5__Sample

tabla2_6 :: forall event. Cycle (Maybe (Note event))
tabla2_6 = cycleFromSample S.tabla2_6__Sample

tabla2_7 :: forall event. Cycle (Maybe (Note event))
tabla2_7 = cycleFromSample S.tabla2_7__Sample

tabla2_8 :: forall event. Cycle (Maybe (Note event))
tabla2_8 = cycleFromSample S.tabla2_8__Sample

tabla2_9 :: forall event. Cycle (Maybe (Note event))
tabla2_9 = cycleFromSample S.tabla2_9__Sample

tabla2_10 :: forall event. Cycle (Maybe (Note event))
tabla2_10 = cycleFromSample S.tabla2_10__Sample

tabla2_11 :: forall event. Cycle (Maybe (Note event))
tabla2_11 = cycleFromSample S.tabla2_11__Sample

tabla2_12 :: forall event. Cycle (Maybe (Note event))
tabla2_12 = cycleFromSample S.tabla2_12__Sample

tabla2_13 :: forall event. Cycle (Maybe (Note event))
tabla2_13 = cycleFromSample S.tabla2_13__Sample

tabla2_14 :: forall event. Cycle (Maybe (Note event))
tabla2_14 = cycleFromSample S.tabla2_14__Sample

tabla2_15 :: forall event. Cycle (Maybe (Note event))
tabla2_15 = cycleFromSample S.tabla2_15__Sample

tabla2_16 :: forall event. Cycle (Maybe (Note event))
tabla2_16 = cycleFromSample S.tabla2_16__Sample

tabla2_17 :: forall event. Cycle (Maybe (Note event))
tabla2_17 = cycleFromSample S.tabla2_17__Sample

tabla2_18 :: forall event. Cycle (Maybe (Note event))
tabla2_18 = cycleFromSample S.tabla2_18__Sample

tabla2_19 :: forall event. Cycle (Maybe (Note event))
tabla2_19 = cycleFromSample S.tabla2_19__Sample

tabla2_20 :: forall event. Cycle (Maybe (Note event))
tabla2_20 = cycleFromSample S.tabla2_20__Sample

tabla2_21 :: forall event. Cycle (Maybe (Note event))
tabla2_21 = cycleFromSample S.tabla2_21__Sample

tabla2_22 :: forall event. Cycle (Maybe (Note event))
tabla2_22 = cycleFromSample S.tabla2_22__Sample

tabla2_23 :: forall event. Cycle (Maybe (Note event))
tabla2_23 = cycleFromSample S.tabla2_23__Sample

tabla2_24 :: forall event. Cycle (Maybe (Note event))
tabla2_24 = cycleFromSample S.tabla2_24__Sample

tabla2_25 :: forall event. Cycle (Maybe (Note event))
tabla2_25 = cycleFromSample S.tabla2_25__Sample

tabla2_26 :: forall event. Cycle (Maybe (Note event))
tabla2_26 = cycleFromSample S.tabla2_26__Sample

tabla2_27 :: forall event. Cycle (Maybe (Note event))
tabla2_27 = cycleFromSample S.tabla2_27__Sample

tabla2_28 :: forall event. Cycle (Maybe (Note event))
tabla2_28 = cycleFromSample S.tabla2_28__Sample

tabla2_29 :: forall event. Cycle (Maybe (Note event))
tabla2_29 = cycleFromSample S.tabla2_29__Sample

tabla2_30 :: forall event. Cycle (Maybe (Note event))
tabla2_30 = cycleFromSample S.tabla2_30__Sample

tabla2_31 :: forall event. Cycle (Maybe (Note event))
tabla2_31 = cycleFromSample S.tabla2_31__Sample

tabla2_32 :: forall event. Cycle (Maybe (Note event))
tabla2_32 = cycleFromSample S.tabla2_32__Sample

tabla2_33 :: forall event. Cycle (Maybe (Note event))
tabla2_33 = cycleFromSample S.tabla2_33__Sample

tabla2_34 :: forall event. Cycle (Maybe (Note event))
tabla2_34 = cycleFromSample S.tabla2_34__Sample

tabla2_35 :: forall event. Cycle (Maybe (Note event))
tabla2_35 = cycleFromSample S.tabla2_35__Sample

tabla2_36 :: forall event. Cycle (Maybe (Note event))
tabla2_36 = cycleFromSample S.tabla2_36__Sample

tabla2_37 :: forall event. Cycle (Maybe (Note event))
tabla2_37 = cycleFromSample S.tabla2_37__Sample

tabla2_38 :: forall event. Cycle (Maybe (Note event))
tabla2_38 = cycleFromSample S.tabla2_38__Sample

tabla2_39 :: forall event. Cycle (Maybe (Note event))
tabla2_39 = cycleFromSample S.tabla2_39__Sample

tabla2_40 :: forall event. Cycle (Maybe (Note event))
tabla2_40 = cycleFromSample S.tabla2_40__Sample

tabla2_41 :: forall event. Cycle (Maybe (Note event))
tabla2_41 = cycleFromSample S.tabla2_41__Sample

tabla2_42 :: forall event. Cycle (Maybe (Note event))
tabla2_42 = cycleFromSample S.tabla2_42__Sample

tabla2_43 :: forall event. Cycle (Maybe (Note event))
tabla2_43 = cycleFromSample S.tabla2_43__Sample

tabla2_44 :: forall event. Cycle (Maybe (Note event))
tabla2_44 = cycleFromSample S.tabla2_44__Sample

tabla2_45 :: forall event. Cycle (Maybe (Note event))
tabla2_45 = cycleFromSample S.tabla2_45__Sample

chin :: forall event. Cycle (Maybe (Note event))
chin = cycleFromSample S.chin_0__Sample

chin_0 :: forall event. Cycle (Maybe (Note event))
chin_0 = cycleFromSample S.chin_0__Sample

chin_1 :: forall event. Cycle (Maybe (Note event))
chin_1 = cycleFromSample S.chin_1__Sample

chin_2 :: forall event. Cycle (Maybe (Note event))
chin_2 = cycleFromSample S.chin_2__Sample

chin_3 :: forall event. Cycle (Maybe (Note event))
chin_3 = cycleFromSample S.chin_3__Sample

mp3 :: forall event. Cycle (Maybe (Note event))
mp3 = cycleFromSample S.mp3_0__Sample

mp3_0 :: forall event. Cycle (Maybe (Note event))
mp3_0 = cycleFromSample S.mp3_0__Sample

mp3_1 :: forall event. Cycle (Maybe (Note event))
mp3_1 = cycleFromSample S.mp3_1__Sample

mp3_2 :: forall event. Cycle (Maybe (Note event))
mp3_2 = cycleFromSample S.mp3_2__Sample

mp3_3 :: forall event. Cycle (Maybe (Note event))
mp3_3 = cycleFromSample S.mp3_3__Sample

tablex :: forall event. Cycle (Maybe (Note event))
tablex = cycleFromSample S.tablex_0__Sample

tablex_0 :: forall event. Cycle (Maybe (Note event))
tablex_0 = cycleFromSample S.tablex_0__Sample

tablex_1 :: forall event. Cycle (Maybe (Note event))
tablex_1 = cycleFromSample S.tablex_1__Sample

tablex_2 :: forall event. Cycle (Maybe (Note event))
tablex_2 = cycleFromSample S.tablex_2__Sample

sf :: forall event. Cycle (Maybe (Note event))
sf = cycleFromSample S.sf_0__Sample

sf_0 :: forall event. Cycle (Maybe (Note event))
sf_0 = cycleFromSample S.sf_0__Sample

sf_1 :: forall event. Cycle (Maybe (Note event))
sf_1 = cycleFromSample S.sf_1__Sample

sf_2 :: forall event. Cycle (Maybe (Note event))
sf_2 = cycleFromSample S.sf_2__Sample

sf_3 :: forall event. Cycle (Maybe (Note event))
sf_3 = cycleFromSample S.sf_3__Sample

sf_4 :: forall event. Cycle (Maybe (Note event))
sf_4 = cycleFromSample S.sf_4__Sample

sf_5 :: forall event. Cycle (Maybe (Note event))
sf_5 = cycleFromSample S.sf_5__Sample

sf_6 :: forall event. Cycle (Maybe (Note event))
sf_6 = cycleFromSample S.sf_6__Sample

sf_7 :: forall event. Cycle (Maybe (Note event))
sf_7 = cycleFromSample S.sf_7__Sample

sf_8 :: forall event. Cycle (Maybe (Note event))
sf_8 = cycleFromSample S.sf_8__Sample

sf_9 :: forall event. Cycle (Maybe (Note event))
sf_9 = cycleFromSample S.sf_9__Sample

sf_10 :: forall event. Cycle (Maybe (Note event))
sf_10 = cycleFromSample S.sf_10__Sample

sf_11 :: forall event. Cycle (Maybe (Note event))
sf_11 = cycleFromSample S.sf_11__Sample

sf_12 :: forall event. Cycle (Maybe (Note event))
sf_12 = cycleFromSample S.sf_12__Sample

sf_13 :: forall event. Cycle (Maybe (Note event))
sf_13 = cycleFromSample S.sf_13__Sample

sf_14 :: forall event. Cycle (Maybe (Note event))
sf_14 = cycleFromSample S.sf_14__Sample

sf_15 :: forall event. Cycle (Maybe (Note event))
sf_15 = cycleFromSample S.sf_15__Sample

sf_16 :: forall event. Cycle (Maybe (Note event))
sf_16 = cycleFromSample S.sf_16__Sample

sf_17 :: forall event. Cycle (Maybe (Note event))
sf_17 = cycleFromSample S.sf_17__Sample

speakspell :: forall event. Cycle (Maybe (Note event))
speakspell = cycleFromSample S.speakspell_0__Sample

speakspell_0 :: forall event. Cycle (Maybe (Note event))
speakspell_0 = cycleFromSample S.speakspell_0__Sample

speakspell_1 :: forall event. Cycle (Maybe (Note event))
speakspell_1 = cycleFromSample S.speakspell_1__Sample

speakspell_2 :: forall event. Cycle (Maybe (Note event))
speakspell_2 = cycleFromSample S.speakspell_2__Sample

speakspell_3 :: forall event. Cycle (Maybe (Note event))
speakspell_3 = cycleFromSample S.speakspell_3__Sample

speakspell_4 :: forall event. Cycle (Maybe (Note event))
speakspell_4 = cycleFromSample S.speakspell_4__Sample

speakspell_5 :: forall event. Cycle (Maybe (Note event))
speakspell_5 = cycleFromSample S.speakspell_5__Sample

speakspell_6 :: forall event. Cycle (Maybe (Note event))
speakspell_6 = cycleFromSample S.speakspell_6__Sample

speakspell_7 :: forall event. Cycle (Maybe (Note event))
speakspell_7 = cycleFromSample S.speakspell_7__Sample

speakspell_8 :: forall event. Cycle (Maybe (Note event))
speakspell_8 = cycleFromSample S.speakspell_8__Sample

speakspell_9 :: forall event. Cycle (Maybe (Note event))
speakspell_9 = cycleFromSample S.speakspell_9__Sample

speakspell_10 :: forall event. Cycle (Maybe (Note event))
speakspell_10 = cycleFromSample S.speakspell_10__Sample

speakspell_11 :: forall event. Cycle (Maybe (Note event))
speakspell_11 = cycleFromSample S.speakspell_11__Sample

cc :: forall event. Cycle (Maybe (Note event))
cc = cycleFromSample S.cc_0__Sample

cc_0 :: forall event. Cycle (Maybe (Note event))
cc_0 = cycleFromSample S.cc_0__Sample

cc_1 :: forall event. Cycle (Maybe (Note event))
cc_1 = cycleFromSample S.cc_1__Sample

cc_2 :: forall event. Cycle (Maybe (Note event))
cc_2 = cycleFromSample S.cc_2__Sample

cc_3 :: forall event. Cycle (Maybe (Note event))
cc_3 = cycleFromSample S.cc_3__Sample

cc_4 :: forall event. Cycle (Maybe (Note event))
cc_4 = cycleFromSample S.cc_4__Sample

cc_5 :: forall event. Cycle (Maybe (Note event))
cc_5 = cycleFromSample S.cc_5__Sample

gabbaloud :: forall event. Cycle (Maybe (Note event))
gabbaloud = cycleFromSample S.gabbaloud_0__Sample

gabbaloud_0 :: forall event. Cycle (Maybe (Note event))
gabbaloud_0 = cycleFromSample S.gabbaloud_0__Sample

gabbaloud_1 :: forall event. Cycle (Maybe (Note event))
gabbaloud_1 = cycleFromSample S.gabbaloud_1__Sample

gabbaloud_2 :: forall event. Cycle (Maybe (Note event))
gabbaloud_2 = cycleFromSample S.gabbaloud_2__Sample

gabbaloud_3 :: forall event. Cycle (Maybe (Note event))
gabbaloud_3 = cycleFromSample S.gabbaloud_3__Sample

ades2 :: forall event. Cycle (Maybe (Note event))
ades2 = cycleFromSample S.ades2_0__Sample

ades2_0 :: forall event. Cycle (Maybe (Note event))
ades2_0 = cycleFromSample S.ades2_0__Sample

ades2_1 :: forall event. Cycle (Maybe (Note event))
ades2_1 = cycleFromSample S.ades2_1__Sample

ades2_2 :: forall event. Cycle (Maybe (Note event))
ades2_2 = cycleFromSample S.ades2_2__Sample

ades2_3 :: forall event. Cycle (Maybe (Note event))
ades2_3 = cycleFromSample S.ades2_3__Sample

ades2_4 :: forall event. Cycle (Maybe (Note event))
ades2_4 = cycleFromSample S.ades2_4__Sample

ades2_5 :: forall event. Cycle (Maybe (Note event))
ades2_5 = cycleFromSample S.ades2_5__Sample

ades2_6 :: forall event. Cycle (Maybe (Note event))
ades2_6 = cycleFromSample S.ades2_6__Sample

ades2_7 :: forall event. Cycle (Maybe (Note event))
ades2_7 = cycleFromSample S.ades2_7__Sample

ades2_8 :: forall event. Cycle (Maybe (Note event))
ades2_8 = cycleFromSample S.ades2_8__Sample

space :: forall event. Cycle (Maybe (Note event))
space = cycleFromSample S.space_0__Sample

space_0 :: forall event. Cycle (Maybe (Note event))
space_0 = cycleFromSample S.space_0__Sample

space_1 :: forall event. Cycle (Maybe (Note event))
space_1 = cycleFromSample S.space_1__Sample

space_2 :: forall event. Cycle (Maybe (Note event))
space_2 = cycleFromSample S.space_2__Sample

space_3 :: forall event. Cycle (Maybe (Note event))
space_3 = cycleFromSample S.space_3__Sample

space_4 :: forall event. Cycle (Maybe (Note event))
space_4 = cycleFromSample S.space_4__Sample

space_5 :: forall event. Cycle (Maybe (Note event))
space_5 = cycleFromSample S.space_5__Sample

space_6 :: forall event. Cycle (Maybe (Note event))
space_6 = cycleFromSample S.space_6__Sample

space_7 :: forall event. Cycle (Maybe (Note event))
space_7 = cycleFromSample S.space_7__Sample

space_8 :: forall event. Cycle (Maybe (Note event))
space_8 = cycleFromSample S.space_8__Sample

space_9 :: forall event. Cycle (Maybe (Note event))
space_9 = cycleFromSample S.space_9__Sample

space_10 :: forall event. Cycle (Maybe (Note event))
space_10 = cycleFromSample S.space_10__Sample

space_11 :: forall event. Cycle (Maybe (Note event))
space_11 = cycleFromSample S.space_11__Sample

space_12 :: forall event. Cycle (Maybe (Note event))
space_12 = cycleFromSample S.space_12__Sample

space_13 :: forall event. Cycle (Maybe (Note event))
space_13 = cycleFromSample S.space_13__Sample

space_14 :: forall event. Cycle (Maybe (Note event))
space_14 = cycleFromSample S.space_14__Sample

space_15 :: forall event. Cycle (Maybe (Note event))
space_15 = cycleFromSample S.space_15__Sample

space_16 :: forall event. Cycle (Maybe (Note event))
space_16 = cycleFromSample S.space_16__Sample

space_17 :: forall event. Cycle (Maybe (Note event))
space_17 = cycleFromSample S.space_17__Sample

battles :: forall event. Cycle (Maybe (Note event))
battles = cycleFromSample S.battles_0__Sample

battles_0 :: forall event. Cycle (Maybe (Note event))
battles_0 = cycleFromSample S.battles_0__Sample

battles_1 :: forall event. Cycle (Maybe (Note event))
battles_1 = cycleFromSample S.battles_1__Sample

voodoo :: forall event. Cycle (Maybe (Note event))
voodoo = cycleFromSample S.voodoo_0__Sample

voodoo_0 :: forall event. Cycle (Maybe (Note event))
voodoo_0 = cycleFromSample S.voodoo_0__Sample

voodoo_1 :: forall event. Cycle (Maybe (Note event))
voodoo_1 = cycleFromSample S.voodoo_1__Sample

voodoo_2 :: forall event. Cycle (Maybe (Note event))
voodoo_2 = cycleFromSample S.voodoo_2__Sample

voodoo_3 :: forall event. Cycle (Maybe (Note event))
voodoo_3 = cycleFromSample S.voodoo_3__Sample

voodoo_4 :: forall event. Cycle (Maybe (Note event))
voodoo_4 = cycleFromSample S.voodoo_4__Sample

ravemono :: forall event. Cycle (Maybe (Note event))
ravemono = cycleFromSample S.ravemono_0__Sample

ravemono_0 :: forall event. Cycle (Maybe (Note event))
ravemono_0 = cycleFromSample S.ravemono_0__Sample

ravemono_1 :: forall event. Cycle (Maybe (Note event))
ravemono_1 = cycleFromSample S.ravemono_1__Sample

psr :: forall event. Cycle (Maybe (Note event))
psr = cycleFromSample S.psr_0__Sample

psr_0 :: forall event. Cycle (Maybe (Note event))
psr_0 = cycleFromSample S.psr_0__Sample

psr_1 :: forall event. Cycle (Maybe (Note event))
psr_1 = cycleFromSample S.psr_1__Sample

psr_2 :: forall event. Cycle (Maybe (Note event))
psr_2 = cycleFromSample S.psr_2__Sample

psr_3 :: forall event. Cycle (Maybe (Note event))
psr_3 = cycleFromSample S.psr_3__Sample

psr_4 :: forall event. Cycle (Maybe (Note event))
psr_4 = cycleFromSample S.psr_4__Sample

psr_5 :: forall event. Cycle (Maybe (Note event))
psr_5 = cycleFromSample S.psr_5__Sample

psr_6 :: forall event. Cycle (Maybe (Note event))
psr_6 = cycleFromSample S.psr_6__Sample

psr_7 :: forall event. Cycle (Maybe (Note event))
psr_7 = cycleFromSample S.psr_7__Sample

psr_8 :: forall event. Cycle (Maybe (Note event))
psr_8 = cycleFromSample S.psr_8__Sample

psr_9 :: forall event. Cycle (Maybe (Note event))
psr_9 = cycleFromSample S.psr_9__Sample

psr_10 :: forall event. Cycle (Maybe (Note event))
psr_10 = cycleFromSample S.psr_10__Sample

psr_11 :: forall event. Cycle (Maybe (Note event))
psr_11 = cycleFromSample S.psr_11__Sample

psr_12 :: forall event. Cycle (Maybe (Note event))
psr_12 = cycleFromSample S.psr_12__Sample

psr_13 :: forall event. Cycle (Maybe (Note event))
psr_13 = cycleFromSample S.psr_13__Sample

psr_14 :: forall event. Cycle (Maybe (Note event))
psr_14 = cycleFromSample S.psr_14__Sample

psr_15 :: forall event. Cycle (Maybe (Note event))
psr_15 = cycleFromSample S.psr_15__Sample

psr_16 :: forall event. Cycle (Maybe (Note event))
psr_16 = cycleFromSample S.psr_16__Sample

psr_17 :: forall event. Cycle (Maybe (Note event))
psr_17 = cycleFromSample S.psr_17__Sample

psr_18 :: forall event. Cycle (Maybe (Note event))
psr_18 = cycleFromSample S.psr_18__Sample

psr_19 :: forall event. Cycle (Maybe (Note event))
psr_19 = cycleFromSample S.psr_19__Sample

psr_20 :: forall event. Cycle (Maybe (Note event))
psr_20 = cycleFromSample S.psr_20__Sample

psr_21 :: forall event. Cycle (Maybe (Note event))
psr_21 = cycleFromSample S.psr_21__Sample

psr_22 :: forall event. Cycle (Maybe (Note event))
psr_22 = cycleFromSample S.psr_22__Sample

psr_23 :: forall event. Cycle (Maybe (Note event))
psr_23 = cycleFromSample S.psr_23__Sample

psr_24 :: forall event. Cycle (Maybe (Note event))
psr_24 = cycleFromSample S.psr_24__Sample

psr_25 :: forall event. Cycle (Maybe (Note event))
psr_25 = cycleFromSample S.psr_25__Sample

psr_26 :: forall event. Cycle (Maybe (Note event))
psr_26 = cycleFromSample S.psr_26__Sample

psr_27 :: forall event. Cycle (Maybe (Note event))
psr_27 = cycleFromSample S.psr_27__Sample

psr_28 :: forall event. Cycle (Maybe (Note event))
psr_28 = cycleFromSample S.psr_28__Sample

psr_29 :: forall event. Cycle (Maybe (Note event))
psr_29 = cycleFromSample S.psr_29__Sample

metal :: forall event. Cycle (Maybe (Note event))
metal = cycleFromSample S.metal_0__Sample

metal_0 :: forall event. Cycle (Maybe (Note event))
metal_0 = cycleFromSample S.metal_0__Sample

metal_1 :: forall event. Cycle (Maybe (Note event))
metal_1 = cycleFromSample S.metal_1__Sample

metal_2 :: forall event. Cycle (Maybe (Note event))
metal_2 = cycleFromSample S.metal_2__Sample

metal_3 :: forall event. Cycle (Maybe (Note event))
metal_3 = cycleFromSample S.metal_3__Sample

metal_4 :: forall event. Cycle (Maybe (Note event))
metal_4 = cycleFromSample S.metal_4__Sample

metal_5 :: forall event. Cycle (Maybe (Note event))
metal_5 = cycleFromSample S.metal_5__Sample

metal_6 :: forall event. Cycle (Maybe (Note event))
metal_6 = cycleFromSample S.metal_6__Sample

metal_7 :: forall event. Cycle (Maybe (Note event))
metal_7 = cycleFromSample S.metal_7__Sample

metal_8 :: forall event. Cycle (Maybe (Note event))
metal_8 = cycleFromSample S.metal_8__Sample

metal_9 :: forall event. Cycle (Maybe (Note event))
metal_9 = cycleFromSample S.metal_9__Sample

hardcore :: forall event. Cycle (Maybe (Note event))
hardcore = cycleFromSample S.hardcore_0__Sample

hardcore_0 :: forall event. Cycle (Maybe (Note event))
hardcore_0 = cycleFromSample S.hardcore_0__Sample

hardcore_1 :: forall event. Cycle (Maybe (Note event))
hardcore_1 = cycleFromSample S.hardcore_1__Sample

hardcore_2 :: forall event. Cycle (Maybe (Note event))
hardcore_2 = cycleFromSample S.hardcore_2__Sample

hardcore_3 :: forall event. Cycle (Maybe (Note event))
hardcore_3 = cycleFromSample S.hardcore_3__Sample

hardcore_4 :: forall event. Cycle (Maybe (Note event))
hardcore_4 = cycleFromSample S.hardcore_4__Sample

hardcore_5 :: forall event. Cycle (Maybe (Note event))
hardcore_5 = cycleFromSample S.hardcore_5__Sample

hardcore_6 :: forall event. Cycle (Maybe (Note event))
hardcore_6 = cycleFromSample S.hardcore_6__Sample

hardcore_7 :: forall event. Cycle (Maybe (Note event))
hardcore_7 = cycleFromSample S.hardcore_7__Sample

hardcore_8 :: forall event. Cycle (Maybe (Note event))
hardcore_8 = cycleFromSample S.hardcore_8__Sample

hardcore_9 :: forall event. Cycle (Maybe (Note event))
hardcore_9 = cycleFromSample S.hardcore_9__Sample

hardcore_10 :: forall event. Cycle (Maybe (Note event))
hardcore_10 = cycleFromSample S.hardcore_10__Sample

hardcore_11 :: forall event. Cycle (Maybe (Note event))
hardcore_11 = cycleFromSample S.hardcore_11__Sample

mouth :: forall event. Cycle (Maybe (Note event))
mouth = cycleFromSample S.mouth_0__Sample

mouth_0 :: forall event. Cycle (Maybe (Note event))
mouth_0 = cycleFromSample S.mouth_0__Sample

mouth_1 :: forall event. Cycle (Maybe (Note event))
mouth_1 = cycleFromSample S.mouth_1__Sample

mouth_2 :: forall event. Cycle (Maybe (Note event))
mouth_2 = cycleFromSample S.mouth_2__Sample

mouth_3 :: forall event. Cycle (Maybe (Note event))
mouth_3 = cycleFromSample S.mouth_3__Sample

mouth_4 :: forall event. Cycle (Maybe (Note event))
mouth_4 = cycleFromSample S.mouth_4__Sample

mouth_5 :: forall event. Cycle (Maybe (Note event))
mouth_5 = cycleFromSample S.mouth_5__Sample

mouth_6 :: forall event. Cycle (Maybe (Note event))
mouth_6 = cycleFromSample S.mouth_6__Sample

mouth_7 :: forall event. Cycle (Maybe (Note event))
mouth_7 = cycleFromSample S.mouth_7__Sample

mouth_8 :: forall event. Cycle (Maybe (Note event))
mouth_8 = cycleFromSample S.mouth_8__Sample

mouth_9 :: forall event. Cycle (Maybe (Note event))
mouth_9 = cycleFromSample S.mouth_9__Sample

mouth_10 :: forall event. Cycle (Maybe (Note event))
mouth_10 = cycleFromSample S.mouth_10__Sample

mouth_11 :: forall event. Cycle (Maybe (Note event))
mouth_11 = cycleFromSample S.mouth_11__Sample

mouth_12 :: forall event. Cycle (Maybe (Note event))
mouth_12 = cycleFromSample S.mouth_12__Sample

mouth_13 :: forall event. Cycle (Maybe (Note event))
mouth_13 = cycleFromSample S.mouth_13__Sample

mouth_14 :: forall event. Cycle (Maybe (Note event))
mouth_14 = cycleFromSample S.mouth_14__Sample

sugar :: forall event. Cycle (Maybe (Note event))
sugar = cycleFromSample S.sugar_0__Sample

sugar_0 :: forall event. Cycle (Maybe (Note event))
sugar_0 = cycleFromSample S.sugar_0__Sample

sugar_1 :: forall event. Cycle (Maybe (Note event))
sugar_1 = cycleFromSample S.sugar_1__Sample

odx :: forall event. Cycle (Maybe (Note event))
odx = cycleFromSample S.odx_0__Sample

odx_0 :: forall event. Cycle (Maybe (Note event))
odx_0 = cycleFromSample S.odx_0__Sample

odx_1 :: forall event. Cycle (Maybe (Note event))
odx_1 = cycleFromSample S.odx_1__Sample

odx_2 :: forall event. Cycle (Maybe (Note event))
odx_2 = cycleFromSample S.odx_2__Sample

odx_3 :: forall event. Cycle (Maybe (Note event))
odx_3 = cycleFromSample S.odx_3__Sample

odx_4 :: forall event. Cycle (Maybe (Note event))
odx_4 = cycleFromSample S.odx_4__Sample

odx_5 :: forall event. Cycle (Maybe (Note event))
odx_5 = cycleFromSample S.odx_5__Sample

odx_6 :: forall event. Cycle (Maybe (Note event))
odx_6 = cycleFromSample S.odx_6__Sample

odx_7 :: forall event. Cycle (Maybe (Note event))
odx_7 = cycleFromSample S.odx_7__Sample

odx_8 :: forall event. Cycle (Maybe (Note event))
odx_8 = cycleFromSample S.odx_8__Sample

odx_9 :: forall event. Cycle (Maybe (Note event))
odx_9 = cycleFromSample S.odx_9__Sample

odx_10 :: forall event. Cycle (Maybe (Note event))
odx_10 = cycleFromSample S.odx_10__Sample

odx_11 :: forall event. Cycle (Maybe (Note event))
odx_11 = cycleFromSample S.odx_11__Sample

odx_12 :: forall event. Cycle (Maybe (Note event))
odx_12 = cycleFromSample S.odx_12__Sample

odx_13 :: forall event. Cycle (Maybe (Note event))
odx_13 = cycleFromSample S.odx_13__Sample

odx_14 :: forall event. Cycle (Maybe (Note event))
odx_14 = cycleFromSample S.odx_14__Sample

x_808lc :: forall event. Cycle (Maybe (Note event))
x_808lc = cycleFromSample S.x_808lc_0__Sample

x_808lc_0 :: forall event. Cycle (Maybe (Note event))
x_808lc_0 = cycleFromSample S.x_808lc_0__Sample

x_808lc_1 :: forall event. Cycle (Maybe (Note event))
x_808lc_1 = cycleFromSample S.x_808lc_1__Sample

x_808lc_2 :: forall event. Cycle (Maybe (Note event))
x_808lc_2 = cycleFromSample S.x_808lc_2__Sample

x_808lc_3 :: forall event. Cycle (Maybe (Note event))
x_808lc_3 = cycleFromSample S.x_808lc_3__Sample

x_808lc_4 :: forall event. Cycle (Maybe (Note event))
x_808lc_4 = cycleFromSample S.x_808lc_4__Sample

mt :: forall event. Cycle (Maybe (Note event))
mt = cycleFromSample S.mt_0__Sample

mt_0 :: forall event. Cycle (Maybe (Note event))
mt_0 = cycleFromSample S.mt_0__Sample

mt_1 :: forall event. Cycle (Maybe (Note event))
mt_1 = cycleFromSample S.mt_1__Sample

mt_2 :: forall event. Cycle (Maybe (Note event))
mt_2 = cycleFromSample S.mt_2__Sample

mt_3 :: forall event. Cycle (Maybe (Note event))
mt_3 = cycleFromSample S.mt_3__Sample

mt_4 :: forall event. Cycle (Maybe (Note event))
mt_4 = cycleFromSample S.mt_4__Sample

mt_5 :: forall event. Cycle (Maybe (Note event))
mt_5 = cycleFromSample S.mt_5__Sample

mt_6 :: forall event. Cycle (Maybe (Note event))
mt_6 = cycleFromSample S.mt_6__Sample

mt_7 :: forall event. Cycle (Maybe (Note event))
mt_7 = cycleFromSample S.mt_7__Sample

mt_8 :: forall event. Cycle (Maybe (Note event))
mt_8 = cycleFromSample S.mt_8__Sample

mt_9 :: forall event. Cycle (Maybe (Note event))
mt_9 = cycleFromSample S.mt_9__Sample

mt_10 :: forall event. Cycle (Maybe (Note event))
mt_10 = cycleFromSample S.mt_10__Sample

mt_11 :: forall event. Cycle (Maybe (Note event))
mt_11 = cycleFromSample S.mt_11__Sample

mt_12 :: forall event. Cycle (Maybe (Note event))
mt_12 = cycleFromSample S.mt_12__Sample

mt_13 :: forall event. Cycle (Maybe (Note event))
mt_13 = cycleFromSample S.mt_13__Sample

mt_14 :: forall event. Cycle (Maybe (Note event))
mt_14 = cycleFromSample S.mt_14__Sample

mt_15 :: forall event. Cycle (Maybe (Note event))
mt_15 = cycleFromSample S.mt_15__Sample

drumtraks :: forall event. Cycle (Maybe (Note event))
drumtraks = cycleFromSample S.drumtraks_0__Sample

drumtraks_0 :: forall event. Cycle (Maybe (Note event))
drumtraks_0 = cycleFromSample S.drumtraks_0__Sample

drumtraks_1 :: forall event. Cycle (Maybe (Note event))
drumtraks_1 = cycleFromSample S.drumtraks_1__Sample

drumtraks_2 :: forall event. Cycle (Maybe (Note event))
drumtraks_2 = cycleFromSample S.drumtraks_2__Sample

drumtraks_3 :: forall event. Cycle (Maybe (Note event))
drumtraks_3 = cycleFromSample S.drumtraks_3__Sample

drumtraks_4 :: forall event. Cycle (Maybe (Note event))
drumtraks_4 = cycleFromSample S.drumtraks_4__Sample

drumtraks_5 :: forall event. Cycle (Maybe (Note event))
drumtraks_5 = cycleFromSample S.drumtraks_5__Sample

drumtraks_6 :: forall event. Cycle (Maybe (Note event))
drumtraks_6 = cycleFromSample S.drumtraks_6__Sample

drumtraks_7 :: forall event. Cycle (Maybe (Note event))
drumtraks_7 = cycleFromSample S.drumtraks_7__Sample

drumtraks_8 :: forall event. Cycle (Maybe (Note event))
drumtraks_8 = cycleFromSample S.drumtraks_8__Sample

drumtraks_9 :: forall event. Cycle (Maybe (Note event))
drumtraks_9 = cycleFromSample S.drumtraks_9__Sample

drumtraks_10 :: forall event. Cycle (Maybe (Note event))
drumtraks_10 = cycleFromSample S.drumtraks_10__Sample

drumtraks_11 :: forall event. Cycle (Maybe (Note event))
drumtraks_11 = cycleFromSample S.drumtraks_11__Sample

drumtraks_12 :: forall event. Cycle (Maybe (Note event))
drumtraks_12 = cycleFromSample S.drumtraks_12__Sample

print :: forall event. Cycle (Maybe (Note event))
print = cycleFromSample S.print_0__Sample

print_0 :: forall event. Cycle (Maybe (Note event))
print_0 = cycleFromSample S.print_0__Sample

print_1 :: forall event. Cycle (Maybe (Note event))
print_1 = cycleFromSample S.print_1__Sample

print_2 :: forall event. Cycle (Maybe (Note event))
print_2 = cycleFromSample S.print_2__Sample

print_3 :: forall event. Cycle (Maybe (Note event))
print_3 = cycleFromSample S.print_3__Sample

print_4 :: forall event. Cycle (Maybe (Note event))
print_4 = cycleFromSample S.print_4__Sample

print_5 :: forall event. Cycle (Maybe (Note event))
print_5 = cycleFromSample S.print_5__Sample

print_6 :: forall event. Cycle (Maybe (Note event))
print_6 = cycleFromSample S.print_6__Sample

print_7 :: forall event. Cycle (Maybe (Note event))
print_7 = cycleFromSample S.print_7__Sample

print_8 :: forall event. Cycle (Maybe (Note event))
print_8 = cycleFromSample S.print_8__Sample

print_9 :: forall event. Cycle (Maybe (Note event))
print_9 = cycleFromSample S.print_9__Sample

print_10 :: forall event. Cycle (Maybe (Note event))
print_10 = cycleFromSample S.print_10__Sample

blip :: forall event. Cycle (Maybe (Note event))
blip = cycleFromSample S.blip_0__Sample

blip_0 :: forall event. Cycle (Maybe (Note event))
blip_0 = cycleFromSample S.blip_0__Sample

blip_1 :: forall event. Cycle (Maybe (Note event))
blip_1 = cycleFromSample S.blip_1__Sample

wobble :: forall event. Cycle (Maybe (Note event))
wobble = cycleFromSample S.wobble_0__Sample

wobble_0 :: forall event. Cycle (Maybe (Note event))
wobble_0 = cycleFromSample S.wobble_0__Sample

made :: forall event. Cycle (Maybe (Note event))
made = cycleFromSample S.made_0__Sample

made_0 :: forall event. Cycle (Maybe (Note event))
made_0 = cycleFromSample S.made_0__Sample

made_1 :: forall event. Cycle (Maybe (Note event))
made_1 = cycleFromSample S.made_1__Sample

made_2 :: forall event. Cycle (Maybe (Note event))
made_2 = cycleFromSample S.made_2__Sample

made_3 :: forall event. Cycle (Maybe (Note event))
made_3 = cycleFromSample S.made_3__Sample

made_4 :: forall event. Cycle (Maybe (Note event))
made_4 = cycleFromSample S.made_4__Sample

made_5 :: forall event. Cycle (Maybe (Note event))
made_5 = cycleFromSample S.made_5__Sample

made_6 :: forall event. Cycle (Maybe (Note event))
made_6 = cycleFromSample S.made_6__Sample

bass3 :: forall event. Cycle (Maybe (Note event))
bass3 = cycleFromSample S.bass3_0__Sample

bass3_0 :: forall event. Cycle (Maybe (Note event))
bass3_0 = cycleFromSample S.bass3_0__Sample

bass3_1 :: forall event. Cycle (Maybe (Note event))
bass3_1 = cycleFromSample S.bass3_1__Sample

bass3_2 :: forall event. Cycle (Maybe (Note event))
bass3_2 = cycleFromSample S.bass3_2__Sample

bass3_3 :: forall event. Cycle (Maybe (Note event))
bass3_3 = cycleFromSample S.bass3_3__Sample

bass3_4 :: forall event. Cycle (Maybe (Note event))
bass3_4 = cycleFromSample S.bass3_4__Sample

bass3_5 :: forall event. Cycle (Maybe (Note event))
bass3_5 = cycleFromSample S.bass3_5__Sample

bass3_6 :: forall event. Cycle (Maybe (Note event))
bass3_6 = cycleFromSample S.bass3_6__Sample

bass3_7 :: forall event. Cycle (Maybe (Note event))
bass3_7 = cycleFromSample S.bass3_7__Sample

bass3_8 :: forall event. Cycle (Maybe (Note event))
bass3_8 = cycleFromSample S.bass3_8__Sample

bass3_9 :: forall event. Cycle (Maybe (Note event))
bass3_9 = cycleFromSample S.bass3_9__Sample

bass3_10 :: forall event. Cycle (Maybe (Note event))
bass3_10 = cycleFromSample S.bass3_10__Sample

speechless :: forall event. Cycle (Maybe (Note event))
speechless = cycleFromSample S.speechless_0__Sample

speechless_0 :: forall event. Cycle (Maybe (Note event))
speechless_0 = cycleFromSample S.speechless_0__Sample

speechless_1 :: forall event. Cycle (Maybe (Note event))
speechless_1 = cycleFromSample S.speechless_1__Sample

speechless_2 :: forall event. Cycle (Maybe (Note event))
speechless_2 = cycleFromSample S.speechless_2__Sample

speechless_3 :: forall event. Cycle (Maybe (Note event))
speechless_3 = cycleFromSample S.speechless_3__Sample

speechless_4 :: forall event. Cycle (Maybe (Note event))
speechless_4 = cycleFromSample S.speechless_4__Sample

speechless_5 :: forall event. Cycle (Maybe (Note event))
speechless_5 = cycleFromSample S.speechless_5__Sample

speechless_6 :: forall event. Cycle (Maybe (Note event))
speechless_6 = cycleFromSample S.speechless_6__Sample

speechless_7 :: forall event. Cycle (Maybe (Note event))
speechless_7 = cycleFromSample S.speechless_7__Sample

speechless_8 :: forall event. Cycle (Maybe (Note event))
speechless_8 = cycleFromSample S.speechless_8__Sample

speechless_9 :: forall event. Cycle (Maybe (Note event))
speechless_9 = cycleFromSample S.speechless_9__Sample

sine :: forall event. Cycle (Maybe (Note event))
sine = cycleFromSample S.sine_0__Sample

sine_0 :: forall event. Cycle (Maybe (Note event))
sine_0 = cycleFromSample S.sine_0__Sample

sine_1 :: forall event. Cycle (Maybe (Note event))
sine_1 = cycleFromSample S.sine_1__Sample

sine_2 :: forall event. Cycle (Maybe (Note event))
sine_2 = cycleFromSample S.sine_2__Sample

sine_3 :: forall event. Cycle (Maybe (Note event))
sine_3 = cycleFromSample S.sine_3__Sample

sine_4 :: forall event. Cycle (Maybe (Note event))
sine_4 = cycleFromSample S.sine_4__Sample

sine_5 :: forall event. Cycle (Maybe (Note event))
sine_5 = cycleFromSample S.sine_5__Sample

noise :: forall event. Cycle (Maybe (Note event))
noise = cycleFromSample S.noise_0__Sample

noise_0 :: forall event. Cycle (Maybe (Note event))
noise_0 = cycleFromSample S.noise_0__Sample

x_808lt :: forall event. Cycle (Maybe (Note event))
x_808lt = cycleFromSample S.x_808lt_0__Sample

x_808lt_0 :: forall event. Cycle (Maybe (Note event))
x_808lt_0 = cycleFromSample S.x_808lt_0__Sample

x_808lt_1 :: forall event. Cycle (Maybe (Note event))
x_808lt_1 = cycleFromSample S.x_808lt_1__Sample

x_808lt_2 :: forall event. Cycle (Maybe (Note event))
x_808lt_2 = cycleFromSample S.x_808lt_2__Sample

x_808lt_3 :: forall event. Cycle (Maybe (Note event))
x_808lt_3 = cycleFromSample S.x_808lt_3__Sample

x_808lt_4 :: forall event. Cycle (Maybe (Note event))
x_808lt_4 = cycleFromSample S.x_808lt_4__Sample

sd :: forall event. Cycle (Maybe (Note event))
sd = cycleFromSample S.sd_0__Sample

sd_0 :: forall event. Cycle (Maybe (Note event))
sd_0 = cycleFromSample S.sd_0__Sample

sd_1 :: forall event. Cycle (Maybe (Note event))
sd_1 = cycleFromSample S.sd_1__Sample

alphabet :: forall event. Cycle (Maybe (Note event))
alphabet = cycleFromSample S.alphabet_0__Sample

alphabet_0 :: forall event. Cycle (Maybe (Note event))
alphabet_0 = cycleFromSample S.alphabet_0__Sample

alphabet_1 :: forall event. Cycle (Maybe (Note event))
alphabet_1 = cycleFromSample S.alphabet_1__Sample

alphabet_2 :: forall event. Cycle (Maybe (Note event))
alphabet_2 = cycleFromSample S.alphabet_2__Sample

alphabet_3 :: forall event. Cycle (Maybe (Note event))
alphabet_3 = cycleFromSample S.alphabet_3__Sample

alphabet_4 :: forall event. Cycle (Maybe (Note event))
alphabet_4 = cycleFromSample S.alphabet_4__Sample

alphabet_5 :: forall event. Cycle (Maybe (Note event))
alphabet_5 = cycleFromSample S.alphabet_5__Sample

alphabet_6 :: forall event. Cycle (Maybe (Note event))
alphabet_6 = cycleFromSample S.alphabet_6__Sample

alphabet_7 :: forall event. Cycle (Maybe (Note event))
alphabet_7 = cycleFromSample S.alphabet_7__Sample

alphabet_8 :: forall event. Cycle (Maybe (Note event))
alphabet_8 = cycleFromSample S.alphabet_8__Sample

alphabet_9 :: forall event. Cycle (Maybe (Note event))
alphabet_9 = cycleFromSample S.alphabet_9__Sample

alphabet_10 :: forall event. Cycle (Maybe (Note event))
alphabet_10 = cycleFromSample S.alphabet_10__Sample

alphabet_11 :: forall event. Cycle (Maybe (Note event))
alphabet_11 = cycleFromSample S.alphabet_11__Sample

alphabet_12 :: forall event. Cycle (Maybe (Note event))
alphabet_12 = cycleFromSample S.alphabet_12__Sample

alphabet_13 :: forall event. Cycle (Maybe (Note event))
alphabet_13 = cycleFromSample S.alphabet_13__Sample

alphabet_14 :: forall event. Cycle (Maybe (Note event))
alphabet_14 = cycleFromSample S.alphabet_14__Sample

alphabet_15 :: forall event. Cycle (Maybe (Note event))
alphabet_15 = cycleFromSample S.alphabet_15__Sample

alphabet_16 :: forall event. Cycle (Maybe (Note event))
alphabet_16 = cycleFromSample S.alphabet_16__Sample

alphabet_17 :: forall event. Cycle (Maybe (Note event))
alphabet_17 = cycleFromSample S.alphabet_17__Sample

alphabet_18 :: forall event. Cycle (Maybe (Note event))
alphabet_18 = cycleFromSample S.alphabet_18__Sample

alphabet_19 :: forall event. Cycle (Maybe (Note event))
alphabet_19 = cycleFromSample S.alphabet_19__Sample

alphabet_20 :: forall event. Cycle (Maybe (Note event))
alphabet_20 = cycleFromSample S.alphabet_20__Sample

alphabet_21 :: forall event. Cycle (Maybe (Note event))
alphabet_21 = cycleFromSample S.alphabet_21__Sample

alphabet_22 :: forall event. Cycle (Maybe (Note event))
alphabet_22 = cycleFromSample S.alphabet_22__Sample

alphabet_23 :: forall event. Cycle (Maybe (Note event))
alphabet_23 = cycleFromSample S.alphabet_23__Sample

alphabet_24 :: forall event. Cycle (Maybe (Note event))
alphabet_24 = cycleFromSample S.alphabet_24__Sample

alphabet_25 :: forall event. Cycle (Maybe (Note event))
alphabet_25 = cycleFromSample S.alphabet_25__Sample

baa2 :: forall event. Cycle (Maybe (Note event))
baa2 = cycleFromSample S.baa2_0__Sample

baa2_0 :: forall event. Cycle (Maybe (Note event))
baa2_0 = cycleFromSample S.baa2_0__Sample

baa2_1 :: forall event. Cycle (Maybe (Note event))
baa2_1 = cycleFromSample S.baa2_1__Sample

baa2_2 :: forall event. Cycle (Maybe (Note event))
baa2_2 = cycleFromSample S.baa2_2__Sample

baa2_3 :: forall event. Cycle (Maybe (Note event))
baa2_3 = cycleFromSample S.baa2_3__Sample

baa2_4 :: forall event. Cycle (Maybe (Note event))
baa2_4 = cycleFromSample S.baa2_4__Sample

baa2_5 :: forall event. Cycle (Maybe (Note event))
baa2_5 = cycleFromSample S.baa2_5__Sample

baa2_6 :: forall event. Cycle (Maybe (Note event))
baa2_6 = cycleFromSample S.baa2_6__Sample

tok :: forall event. Cycle (Maybe (Note event))
tok = cycleFromSample S.tok_0__Sample

tok_0 :: forall event. Cycle (Maybe (Note event))
tok_0 = cycleFromSample S.tok_0__Sample

tok_1 :: forall event. Cycle (Maybe (Note event))
tok_1 = cycleFromSample S.tok_1__Sample

tok_2 :: forall event. Cycle (Maybe (Note event))
tok_2 = cycleFromSample S.tok_2__Sample

tok_3 :: forall event. Cycle (Maybe (Note event))
tok_3 = cycleFromSample S.tok_3__Sample

ades3 :: forall event. Cycle (Maybe (Note event))
ades3 = cycleFromSample S.ades3_0__Sample

ades3_0 :: forall event. Cycle (Maybe (Note event))
ades3_0 = cycleFromSample S.ades3_0__Sample

ades3_1 :: forall event. Cycle (Maybe (Note event))
ades3_1 = cycleFromSample S.ades3_1__Sample

ades3_2 :: forall event. Cycle (Maybe (Note event))
ades3_2 = cycleFromSample S.ades3_2__Sample

ades3_3 :: forall event. Cycle (Maybe (Note event))
ades3_3 = cycleFromSample S.ades3_3__Sample

ades3_4 :: forall event. Cycle (Maybe (Note event))
ades3_4 = cycleFromSample S.ades3_4__Sample

ades3_5 :: forall event. Cycle (Maybe (Note event))
ades3_5 = cycleFromSample S.ades3_5__Sample

ades3_6 :: forall event. Cycle (Maybe (Note event))
ades3_6 = cycleFromSample S.ades3_6__Sample

x_909 :: forall event. Cycle (Maybe (Note event))
x_909 = cycleFromSample S.x_909_0__Sample

x_909_0 :: forall event. Cycle (Maybe (Note event))
x_909_0 = cycleFromSample S.x_909_0__Sample

sid :: forall event. Cycle (Maybe (Note event))
sid = cycleFromSample S.sid_0__Sample

sid_0 :: forall event. Cycle (Maybe (Note event))
sid_0 = cycleFromSample S.sid_0__Sample

sid_1 :: forall event. Cycle (Maybe (Note event))
sid_1 = cycleFromSample S.sid_1__Sample

sid_2 :: forall event. Cycle (Maybe (Note event))
sid_2 = cycleFromSample S.sid_2__Sample

sid_3 :: forall event. Cycle (Maybe (Note event))
sid_3 = cycleFromSample S.sid_3__Sample

sid_4 :: forall event. Cycle (Maybe (Note event))
sid_4 = cycleFromSample S.sid_4__Sample

sid_5 :: forall event. Cycle (Maybe (Note event))
sid_5 = cycleFromSample S.sid_5__Sample

sid_6 :: forall event. Cycle (Maybe (Note event))
sid_6 = cycleFromSample S.sid_6__Sample

sid_7 :: forall event. Cycle (Maybe (Note event))
sid_7 = cycleFromSample S.sid_7__Sample

sid_8 :: forall event. Cycle (Maybe (Note event))
sid_8 = cycleFromSample S.sid_8__Sample

sid_9 :: forall event. Cycle (Maybe (Note event))
sid_9 = cycleFromSample S.sid_9__Sample

sid_10 :: forall event. Cycle (Maybe (Note event))
sid_10 = cycleFromSample S.sid_10__Sample

sid_11 :: forall event. Cycle (Maybe (Note event))
sid_11 = cycleFromSample S.sid_11__Sample

jungbass :: forall event. Cycle (Maybe (Note event))
jungbass = cycleFromSample S.jungbass_0__Sample

jungbass_0 :: forall event. Cycle (Maybe (Note event))
jungbass_0 = cycleFromSample S.jungbass_0__Sample

jungbass_1 :: forall event. Cycle (Maybe (Note event))
jungbass_1 = cycleFromSample S.jungbass_1__Sample

jungbass_2 :: forall event. Cycle (Maybe (Note event))
jungbass_2 = cycleFromSample S.jungbass_2__Sample

jungbass_3 :: forall event. Cycle (Maybe (Note event))
jungbass_3 = cycleFromSample S.jungbass_3__Sample

jungbass_4 :: forall event. Cycle (Maybe (Note event))
jungbass_4 = cycleFromSample S.jungbass_4__Sample

jungbass_5 :: forall event. Cycle (Maybe (Note event))
jungbass_5 = cycleFromSample S.jungbass_5__Sample

jungbass_6 :: forall event. Cycle (Maybe (Note event))
jungbass_6 = cycleFromSample S.jungbass_6__Sample

jungbass_7 :: forall event. Cycle (Maybe (Note event))
jungbass_7 = cycleFromSample S.jungbass_7__Sample

jungbass_8 :: forall event. Cycle (Maybe (Note event))
jungbass_8 = cycleFromSample S.jungbass_8__Sample

jungbass_9 :: forall event. Cycle (Maybe (Note event))
jungbass_9 = cycleFromSample S.jungbass_9__Sample

jungbass_10 :: forall event. Cycle (Maybe (Note event))
jungbass_10 = cycleFromSample S.jungbass_10__Sample

jungbass_11 :: forall event. Cycle (Maybe (Note event))
jungbass_11 = cycleFromSample S.jungbass_11__Sample

jungbass_12 :: forall event. Cycle (Maybe (Note event))
jungbass_12 = cycleFromSample S.jungbass_12__Sample

jungbass_13 :: forall event. Cycle (Maybe (Note event))
jungbass_13 = cycleFromSample S.jungbass_13__Sample

jungbass_14 :: forall event. Cycle (Maybe (Note event))
jungbass_14 = cycleFromSample S.jungbass_14__Sample

jungbass_15 :: forall event. Cycle (Maybe (Note event))
jungbass_15 = cycleFromSample S.jungbass_15__Sample

jungbass_16 :: forall event. Cycle (Maybe (Note event))
jungbass_16 = cycleFromSample S.jungbass_16__Sample

jungbass_17 :: forall event. Cycle (Maybe (Note event))
jungbass_17 = cycleFromSample S.jungbass_17__Sample

jungbass_18 :: forall event. Cycle (Maybe (Note event))
jungbass_18 = cycleFromSample S.jungbass_18__Sample

jungbass_19 :: forall event. Cycle (Maybe (Note event))
jungbass_19 = cycleFromSample S.jungbass_19__Sample

gabba :: forall event. Cycle (Maybe (Note event))
gabba = cycleFromSample S.gabba_0__Sample

gabba_0 :: forall event. Cycle (Maybe (Note event))
gabba_0 = cycleFromSample S.gabba_0__Sample

gabba_1 :: forall event. Cycle (Maybe (Note event))
gabba_1 = cycleFromSample S.gabba_1__Sample

gabba_2 :: forall event. Cycle (Maybe (Note event))
gabba_2 = cycleFromSample S.gabba_2__Sample

gabba_3 :: forall event. Cycle (Maybe (Note event))
gabba_3 = cycleFromSample S.gabba_3__Sample

crow :: forall event. Cycle (Maybe (Note event))
crow = cycleFromSample S.crow_0__Sample

crow_0 :: forall event. Cycle (Maybe (Note event))
crow_0 = cycleFromSample S.crow_0__Sample

crow_1 :: forall event. Cycle (Maybe (Note event))
crow_1 = cycleFromSample S.crow_1__Sample

crow_2 :: forall event. Cycle (Maybe (Note event))
crow_2 = cycleFromSample S.crow_2__Sample

crow_3 :: forall event. Cycle (Maybe (Note event))
crow_3 = cycleFromSample S.crow_3__Sample

birds3 :: forall event. Cycle (Maybe (Note event))
birds3 = cycleFromSample S.birds3_0__Sample

birds3_0 :: forall event. Cycle (Maybe (Note event))
birds3_0 = cycleFromSample S.birds3_0__Sample

birds3_1 :: forall event. Cycle (Maybe (Note event))
birds3_1 = cycleFromSample S.birds3_1__Sample

birds3_2 :: forall event. Cycle (Maybe (Note event))
birds3_2 = cycleFromSample S.birds3_2__Sample

birds3_3 :: forall event. Cycle (Maybe (Note event))
birds3_3 = cycleFromSample S.birds3_3__Sample

birds3_4 :: forall event. Cycle (Maybe (Note event))
birds3_4 = cycleFromSample S.birds3_4__Sample

birds3_5 :: forall event. Cycle (Maybe (Note event))
birds3_5 = cycleFromSample S.birds3_5__Sample

birds3_6 :: forall event. Cycle (Maybe (Note event))
birds3_6 = cycleFromSample S.birds3_6__Sample

birds3_7 :: forall event. Cycle (Maybe (Note event))
birds3_7 = cycleFromSample S.birds3_7__Sample

birds3_8 :: forall event. Cycle (Maybe (Note event))
birds3_8 = cycleFromSample S.birds3_8__Sample

birds3_9 :: forall event. Cycle (Maybe (Note event))
birds3_9 = cycleFromSample S.birds3_9__Sample

birds3_10 :: forall event. Cycle (Maybe (Note event))
birds3_10 = cycleFromSample S.birds3_10__Sample

birds3_11 :: forall event. Cycle (Maybe (Note event))
birds3_11 = cycleFromSample S.birds3_11__Sample

birds3_12 :: forall event. Cycle (Maybe (Note event))
birds3_12 = cycleFromSample S.birds3_12__Sample

birds3_13 :: forall event. Cycle (Maybe (Note event))
birds3_13 = cycleFromSample S.birds3_13__Sample

birds3_14 :: forall event. Cycle (Maybe (Note event))
birds3_14 = cycleFromSample S.birds3_14__Sample

birds3_15 :: forall event. Cycle (Maybe (Note event))
birds3_15 = cycleFromSample S.birds3_15__Sample

birds3_16 :: forall event. Cycle (Maybe (Note event))
birds3_16 = cycleFromSample S.birds3_16__Sample

birds3_17 :: forall event. Cycle (Maybe (Note event))
birds3_17 = cycleFromSample S.birds3_17__Sample

birds3_18 :: forall event. Cycle (Maybe (Note event))
birds3_18 = cycleFromSample S.birds3_18__Sample

auto :: forall event. Cycle (Maybe (Note event))
auto = cycleFromSample S.auto_0__Sample

auto_0 :: forall event. Cycle (Maybe (Note event))
auto_0 = cycleFromSample S.auto_0__Sample

auto_1 :: forall event. Cycle (Maybe (Note event))
auto_1 = cycleFromSample S.auto_1__Sample

auto_2 :: forall event. Cycle (Maybe (Note event))
auto_2 = cycleFromSample S.auto_2__Sample

auto_3 :: forall event. Cycle (Maybe (Note event))
auto_3 = cycleFromSample S.auto_3__Sample

auto_4 :: forall event. Cycle (Maybe (Note event))
auto_4 = cycleFromSample S.auto_4__Sample

auto_5 :: forall event. Cycle (Maybe (Note event))
auto_5 = cycleFromSample S.auto_5__Sample

auto_6 :: forall event. Cycle (Maybe (Note event))
auto_6 = cycleFromSample S.auto_6__Sample

auto_7 :: forall event. Cycle (Maybe (Note event))
auto_7 = cycleFromSample S.auto_7__Sample

auto_8 :: forall event. Cycle (Maybe (Note event))
auto_8 = cycleFromSample S.auto_8__Sample

auto_9 :: forall event. Cycle (Maybe (Note event))
auto_9 = cycleFromSample S.auto_9__Sample

auto_10 :: forall event. Cycle (Maybe (Note event))
auto_10 = cycleFromSample S.auto_10__Sample

mute :: forall event. Cycle (Maybe (Note event))
mute = cycleFromSample S.mute_0__Sample

mute_0 :: forall event. Cycle (Maybe (Note event))
mute_0 = cycleFromSample S.mute_0__Sample

mute_1 :: forall event. Cycle (Maybe (Note event))
mute_1 = cycleFromSample S.mute_1__Sample

mute_2 :: forall event. Cycle (Maybe (Note event))
mute_2 = cycleFromSample S.mute_2__Sample

mute_3 :: forall event. Cycle (Maybe (Note event))
mute_3 = cycleFromSample S.mute_3__Sample

mute_4 :: forall event. Cycle (Maybe (Note event))
mute_4 = cycleFromSample S.mute_4__Sample

mute_5 :: forall event. Cycle (Maybe (Note event))
mute_5 = cycleFromSample S.mute_5__Sample

mute_6 :: forall event. Cycle (Maybe (Note event))
mute_6 = cycleFromSample S.mute_6__Sample

mute_7 :: forall event. Cycle (Maybe (Note event))
mute_7 = cycleFromSample S.mute_7__Sample

mute_8 :: forall event. Cycle (Maybe (Note event))
mute_8 = cycleFromSample S.mute_8__Sample

mute_9 :: forall event. Cycle (Maybe (Note event))
mute_9 = cycleFromSample S.mute_9__Sample

mute_10 :: forall event. Cycle (Maybe (Note event))
mute_10 = cycleFromSample S.mute_10__Sample

mute_11 :: forall event. Cycle (Maybe (Note event))
mute_11 = cycleFromSample S.mute_11__Sample

mute_12 :: forall event. Cycle (Maybe (Note event))
mute_12 = cycleFromSample S.mute_12__Sample

mute_13 :: forall event. Cycle (Maybe (Note event))
mute_13 = cycleFromSample S.mute_13__Sample

mute_14 :: forall event. Cycle (Maybe (Note event))
mute_14 = cycleFromSample S.mute_14__Sample

mute_15 :: forall event. Cycle (Maybe (Note event))
mute_15 = cycleFromSample S.mute_15__Sample

mute_16 :: forall event. Cycle (Maybe (Note event))
mute_16 = cycleFromSample S.mute_16__Sample

mute_17 :: forall event. Cycle (Maybe (Note event))
mute_17 = cycleFromSample S.mute_17__Sample

mute_18 :: forall event. Cycle (Maybe (Note event))
mute_18 = cycleFromSample S.mute_18__Sample

mute_19 :: forall event. Cycle (Maybe (Note event))
mute_19 = cycleFromSample S.mute_19__Sample

mute_20 :: forall event. Cycle (Maybe (Note event))
mute_20 = cycleFromSample S.mute_20__Sample

mute_21 :: forall event. Cycle (Maybe (Note event))
mute_21 = cycleFromSample S.mute_21__Sample

mute_22 :: forall event. Cycle (Maybe (Note event))
mute_22 = cycleFromSample S.mute_22__Sample

mute_23 :: forall event. Cycle (Maybe (Note event))
mute_23 = cycleFromSample S.mute_23__Sample

mute_24 :: forall event. Cycle (Maybe (Note event))
mute_24 = cycleFromSample S.mute_24__Sample

mute_25 :: forall event. Cycle (Maybe (Note event))
mute_25 = cycleFromSample S.mute_25__Sample

mute_26 :: forall event. Cycle (Maybe (Note event))
mute_26 = cycleFromSample S.mute_26__Sample

mute_27 :: forall event. Cycle (Maybe (Note event))
mute_27 = cycleFromSample S.mute_27__Sample

sheffield :: forall event. Cycle (Maybe (Note event))
sheffield = cycleFromSample S.sheffield_0__Sample

sheffield_0 :: forall event. Cycle (Maybe (Note event))
sheffield_0 = cycleFromSample S.sheffield_0__Sample

casio :: forall event. Cycle (Maybe (Note event))
casio = cycleFromSample S.casio_0__Sample

casio_0 :: forall event. Cycle (Maybe (Note event))
casio_0 = cycleFromSample S.casio_0__Sample

casio_1 :: forall event. Cycle (Maybe (Note event))
casio_1 = cycleFromSample S.casio_1__Sample

casio_2 :: forall event. Cycle (Maybe (Note event))
casio_2 = cycleFromSample S.casio_2__Sample

sax :: forall event. Cycle (Maybe (Note event))
sax = cycleFromSample S.sax_0__Sample

sax_0 :: forall event. Cycle (Maybe (Note event))
sax_0 = cycleFromSample S.sax_0__Sample

sax_1 :: forall event. Cycle (Maybe (Note event))
sax_1 = cycleFromSample S.sax_1__Sample

sax_2 :: forall event. Cycle (Maybe (Note event))
sax_2 = cycleFromSample S.sax_2__Sample

sax_3 :: forall event. Cycle (Maybe (Note event))
sax_3 = cycleFromSample S.sax_3__Sample

sax_4 :: forall event. Cycle (Maybe (Note event))
sax_4 = cycleFromSample S.sax_4__Sample

sax_5 :: forall event. Cycle (Maybe (Note event))
sax_5 = cycleFromSample S.sax_5__Sample

sax_6 :: forall event. Cycle (Maybe (Note event))
sax_6 = cycleFromSample S.sax_6__Sample

sax_7 :: forall event. Cycle (Maybe (Note event))
sax_7 = cycleFromSample S.sax_7__Sample

sax_8 :: forall event. Cycle (Maybe (Note event))
sax_8 = cycleFromSample S.sax_8__Sample

sax_9 :: forall event. Cycle (Maybe (Note event))
sax_9 = cycleFromSample S.sax_9__Sample

sax_10 :: forall event. Cycle (Maybe (Note event))
sax_10 = cycleFromSample S.sax_10__Sample

sax_11 :: forall event. Cycle (Maybe (Note event))
sax_11 = cycleFromSample S.sax_11__Sample

sax_12 :: forall event. Cycle (Maybe (Note event))
sax_12 = cycleFromSample S.sax_12__Sample

sax_13 :: forall event. Cycle (Maybe (Note event))
sax_13 = cycleFromSample S.sax_13__Sample

sax_14 :: forall event. Cycle (Maybe (Note event))
sax_14 = cycleFromSample S.sax_14__Sample

sax_15 :: forall event. Cycle (Maybe (Note event))
sax_15 = cycleFromSample S.sax_15__Sample

sax_16 :: forall event. Cycle (Maybe (Note event))
sax_16 = cycleFromSample S.sax_16__Sample

sax_17 :: forall event. Cycle (Maybe (Note event))
sax_17 = cycleFromSample S.sax_17__Sample

sax_18 :: forall event. Cycle (Maybe (Note event))
sax_18 = cycleFromSample S.sax_18__Sample

sax_19 :: forall event. Cycle (Maybe (Note event))
sax_19 = cycleFromSample S.sax_19__Sample

sax_20 :: forall event. Cycle (Maybe (Note event))
sax_20 = cycleFromSample S.sax_20__Sample

sax_21 :: forall event. Cycle (Maybe (Note event))
sax_21 = cycleFromSample S.sax_21__Sample

circus :: forall event. Cycle (Maybe (Note event))
circus = cycleFromSample S.circus_0__Sample

circus_0 :: forall event. Cycle (Maybe (Note event))
circus_0 = cycleFromSample S.circus_0__Sample

circus_1 :: forall event. Cycle (Maybe (Note event))
circus_1 = cycleFromSample S.circus_1__Sample

circus_2 :: forall event. Cycle (Maybe (Note event))
circus_2 = cycleFromSample S.circus_2__Sample

yeah :: forall event. Cycle (Maybe (Note event))
yeah = cycleFromSample S.yeah_0__Sample

yeah_0 :: forall event. Cycle (Maybe (Note event))
yeah_0 = cycleFromSample S.yeah_0__Sample

yeah_1 :: forall event. Cycle (Maybe (Note event))
yeah_1 = cycleFromSample S.yeah_1__Sample

yeah_2 :: forall event. Cycle (Maybe (Note event))
yeah_2 = cycleFromSample S.yeah_2__Sample

yeah_3 :: forall event. Cycle (Maybe (Note event))
yeah_3 = cycleFromSample S.yeah_3__Sample

yeah_4 :: forall event. Cycle (Maybe (Note event))
yeah_4 = cycleFromSample S.yeah_4__Sample

yeah_5 :: forall event. Cycle (Maybe (Note event))
yeah_5 = cycleFromSample S.yeah_5__Sample

yeah_6 :: forall event. Cycle (Maybe (Note event))
yeah_6 = cycleFromSample S.yeah_6__Sample

yeah_7 :: forall event. Cycle (Maybe (Note event))
yeah_7 = cycleFromSample S.yeah_7__Sample

yeah_8 :: forall event. Cycle (Maybe (Note event))
yeah_8 = cycleFromSample S.yeah_8__Sample

yeah_9 :: forall event. Cycle (Maybe (Note event))
yeah_9 = cycleFromSample S.yeah_9__Sample

yeah_10 :: forall event. Cycle (Maybe (Note event))
yeah_10 = cycleFromSample S.yeah_10__Sample

yeah_11 :: forall event. Cycle (Maybe (Note event))
yeah_11 = cycleFromSample S.yeah_11__Sample

yeah_12 :: forall event. Cycle (Maybe (Note event))
yeah_12 = cycleFromSample S.yeah_12__Sample

yeah_13 :: forall event. Cycle (Maybe (Note event))
yeah_13 = cycleFromSample S.yeah_13__Sample

yeah_14 :: forall event. Cycle (Maybe (Note event))
yeah_14 = cycleFromSample S.yeah_14__Sample

yeah_15 :: forall event. Cycle (Maybe (Note event))
yeah_15 = cycleFromSample S.yeah_15__Sample

yeah_16 :: forall event. Cycle (Maybe (Note event))
yeah_16 = cycleFromSample S.yeah_16__Sample

yeah_17 :: forall event. Cycle (Maybe (Note event))
yeah_17 = cycleFromSample S.yeah_17__Sample

yeah_18 :: forall event. Cycle (Maybe (Note event))
yeah_18 = cycleFromSample S.yeah_18__Sample

yeah_19 :: forall event. Cycle (Maybe (Note event))
yeah_19 = cycleFromSample S.yeah_19__Sample

yeah_20 :: forall event. Cycle (Maybe (Note event))
yeah_20 = cycleFromSample S.yeah_20__Sample

yeah_21 :: forall event. Cycle (Maybe (Note event))
yeah_21 = cycleFromSample S.yeah_21__Sample

yeah_22 :: forall event. Cycle (Maybe (Note event))
yeah_22 = cycleFromSample S.yeah_22__Sample

yeah_23 :: forall event. Cycle (Maybe (Note event))
yeah_23 = cycleFromSample S.yeah_23__Sample

yeah_24 :: forall event. Cycle (Maybe (Note event))
yeah_24 = cycleFromSample S.yeah_24__Sample

yeah_25 :: forall event. Cycle (Maybe (Note event))
yeah_25 = cycleFromSample S.yeah_25__Sample

yeah_26 :: forall event. Cycle (Maybe (Note event))
yeah_26 = cycleFromSample S.yeah_26__Sample

yeah_27 :: forall event. Cycle (Maybe (Note event))
yeah_27 = cycleFromSample S.yeah_27__Sample

yeah_28 :: forall event. Cycle (Maybe (Note event))
yeah_28 = cycleFromSample S.yeah_28__Sample

yeah_29 :: forall event. Cycle (Maybe (Note event))
yeah_29 = cycleFromSample S.yeah_29__Sample

yeah_30 :: forall event. Cycle (Maybe (Note event))
yeah_30 = cycleFromSample S.yeah_30__Sample

oc :: forall event. Cycle (Maybe (Note event))
oc = cycleFromSample S.oc_0__Sample

oc_0 :: forall event. Cycle (Maybe (Note event))
oc_0 = cycleFromSample S.oc_0__Sample

oc_1 :: forall event. Cycle (Maybe (Note event))
oc_1 = cycleFromSample S.oc_1__Sample

oc_2 :: forall event. Cycle (Maybe (Note event))
oc_2 = cycleFromSample S.oc_2__Sample

oc_3 :: forall event. Cycle (Maybe (Note event))
oc_3 = cycleFromSample S.oc_3__Sample

alex :: forall event. Cycle (Maybe (Note event))
alex = cycleFromSample S.alex_0__Sample

alex_0 :: forall event. Cycle (Maybe (Note event))
alex_0 = cycleFromSample S.alex_0__Sample

alex_1 :: forall event. Cycle (Maybe (Note event))
alex_1 = cycleFromSample S.alex_1__Sample

can :: forall event. Cycle (Maybe (Note event))
can = cycleFromSample S.can_0__Sample

can_0 :: forall event. Cycle (Maybe (Note event))
can_0 = cycleFromSample S.can_0__Sample

can_1 :: forall event. Cycle (Maybe (Note event))
can_1 = cycleFromSample S.can_1__Sample

can_2 :: forall event. Cycle (Maybe (Note event))
can_2 = cycleFromSample S.can_2__Sample

can_3 :: forall event. Cycle (Maybe (Note event))
can_3 = cycleFromSample S.can_3__Sample

can_4 :: forall event. Cycle (Maybe (Note event))
can_4 = cycleFromSample S.can_4__Sample

can_5 :: forall event. Cycle (Maybe (Note event))
can_5 = cycleFromSample S.can_5__Sample

can_6 :: forall event. Cycle (Maybe (Note event))
can_6 = cycleFromSample S.can_6__Sample

can_7 :: forall event. Cycle (Maybe (Note event))
can_7 = cycleFromSample S.can_7__Sample

can_8 :: forall event. Cycle (Maybe (Note event))
can_8 = cycleFromSample S.can_8__Sample

can_9 :: forall event. Cycle (Maybe (Note event))
can_9 = cycleFromSample S.can_9__Sample

can_10 :: forall event. Cycle (Maybe (Note event))
can_10 = cycleFromSample S.can_10__Sample

can_11 :: forall event. Cycle (Maybe (Note event))
can_11 = cycleFromSample S.can_11__Sample

can_12 :: forall event. Cycle (Maybe (Note event))
can_12 = cycleFromSample S.can_12__Sample

can_13 :: forall event. Cycle (Maybe (Note event))
can_13 = cycleFromSample S.can_13__Sample

jungle :: forall event. Cycle (Maybe (Note event))
jungle = cycleFromSample S.jungle_0__Sample

jungle_0 :: forall event. Cycle (Maybe (Note event))
jungle_0 = cycleFromSample S.jungle_0__Sample

jungle_1 :: forall event. Cycle (Maybe (Note event))
jungle_1 = cycleFromSample S.jungle_1__Sample

jungle_2 :: forall event. Cycle (Maybe (Note event))
jungle_2 = cycleFromSample S.jungle_2__Sample

jungle_3 :: forall event. Cycle (Maybe (Note event))
jungle_3 = cycleFromSample S.jungle_3__Sample

jungle_4 :: forall event. Cycle (Maybe (Note event))
jungle_4 = cycleFromSample S.jungle_4__Sample

jungle_5 :: forall event. Cycle (Maybe (Note event))
jungle_5 = cycleFromSample S.jungle_5__Sample

jungle_6 :: forall event. Cycle (Maybe (Note event))
jungle_6 = cycleFromSample S.jungle_6__Sample

jungle_7 :: forall event. Cycle (Maybe (Note event))
jungle_7 = cycleFromSample S.jungle_7__Sample

jungle_8 :: forall event. Cycle (Maybe (Note event))
jungle_8 = cycleFromSample S.jungle_8__Sample

jungle_9 :: forall event. Cycle (Maybe (Note event))
jungle_9 = cycleFromSample S.jungle_9__Sample

jungle_10 :: forall event. Cycle (Maybe (Note event))
jungle_10 = cycleFromSample S.jungle_10__Sample

jungle_11 :: forall event. Cycle (Maybe (Note event))
jungle_11 = cycleFromSample S.jungle_11__Sample

jungle_12 :: forall event. Cycle (Maybe (Note event))
jungle_12 = cycleFromSample S.jungle_12__Sample

moog :: forall event. Cycle (Maybe (Note event))
moog = cycleFromSample S.moog_0__Sample

moog_0 :: forall event. Cycle (Maybe (Note event))
moog_0 = cycleFromSample S.moog_0__Sample

moog_1 :: forall event. Cycle (Maybe (Note event))
moog_1 = cycleFromSample S.moog_1__Sample

moog_2 :: forall event. Cycle (Maybe (Note event))
moog_2 = cycleFromSample S.moog_2__Sample

moog_3 :: forall event. Cycle (Maybe (Note event))
moog_3 = cycleFromSample S.moog_3__Sample

moog_4 :: forall event. Cycle (Maybe (Note event))
moog_4 = cycleFromSample S.moog_4__Sample

moog_5 :: forall event. Cycle (Maybe (Note event))
moog_5 = cycleFromSample S.moog_5__Sample

moog_6 :: forall event. Cycle (Maybe (Note event))
moog_6 = cycleFromSample S.moog_6__Sample

h :: forall event. Cycle (Maybe (Note event))
h = cycleFromSample S.h_0__Sample

h_0 :: forall event. Cycle (Maybe (Note event))
h_0 = cycleFromSample S.h_0__Sample

h_1 :: forall event. Cycle (Maybe (Note event))
h_1 = cycleFromSample S.h_1__Sample

h_2 :: forall event. Cycle (Maybe (Note event))
h_2 = cycleFromSample S.h_2__Sample

h_3 :: forall event. Cycle (Maybe (Note event))
h_3 = cycleFromSample S.h_3__Sample

h_4 :: forall event. Cycle (Maybe (Note event))
h_4 = cycleFromSample S.h_4__Sample

h_5 :: forall event. Cycle (Maybe (Note event))
h_5 = cycleFromSample S.h_5__Sample

h_6 :: forall event. Cycle (Maybe (Note event))
h_6 = cycleFromSample S.h_6__Sample

wind :: forall event. Cycle (Maybe (Note event))
wind = cycleFromSample S.wind_0__Sample

wind_0 :: forall event. Cycle (Maybe (Note event))
wind_0 = cycleFromSample S.wind_0__Sample

wind_1 :: forall event. Cycle (Maybe (Note event))
wind_1 = cycleFromSample S.wind_1__Sample

wind_2 :: forall event. Cycle (Maybe (Note event))
wind_2 = cycleFromSample S.wind_2__Sample

wind_3 :: forall event. Cycle (Maybe (Note event))
wind_3 = cycleFromSample S.wind_3__Sample

wind_4 :: forall event. Cycle (Maybe (Note event))
wind_4 = cycleFromSample S.wind_4__Sample

wind_5 :: forall event. Cycle (Maybe (Note event))
wind_5 = cycleFromSample S.wind_5__Sample

wind_6 :: forall event. Cycle (Maybe (Note event))
wind_6 = cycleFromSample S.wind_6__Sample

wind_7 :: forall event. Cycle (Maybe (Note event))
wind_7 = cycleFromSample S.wind_7__Sample

wind_8 :: forall event. Cycle (Maybe (Note event))
wind_8 = cycleFromSample S.wind_8__Sample

wind_9 :: forall event. Cycle (Maybe (Note event))
wind_9 = cycleFromSample S.wind_9__Sample

rs :: forall event. Cycle (Maybe (Note event))
rs = cycleFromSample S.rs_0__Sample

rs_0 :: forall event. Cycle (Maybe (Note event))
rs_0 = cycleFromSample S.rs_0__Sample

em2 :: forall event. Cycle (Maybe (Note event))
em2 = cycleFromSample S.em2_0__Sample

em2_0 :: forall event. Cycle (Maybe (Note event))
em2_0 = cycleFromSample S.em2_0__Sample

em2_1 :: forall event. Cycle (Maybe (Note event))
em2_1 = cycleFromSample S.em2_1__Sample

em2_2 :: forall event. Cycle (Maybe (Note event))
em2_2 = cycleFromSample S.em2_2__Sample

em2_3 :: forall event. Cycle (Maybe (Note event))
em2_3 = cycleFromSample S.em2_3__Sample

em2_4 :: forall event. Cycle (Maybe (Note event))
em2_4 = cycleFromSample S.em2_4__Sample

em2_5 :: forall event. Cycle (Maybe (Note event))
em2_5 = cycleFromSample S.em2_5__Sample

noise2 :: forall event. Cycle (Maybe (Note event))
noise2 = cycleFromSample S.noise2_0__Sample

noise2_0 :: forall event. Cycle (Maybe (Note event))
noise2_0 = cycleFromSample S.noise2_0__Sample

noise2_1 :: forall event. Cycle (Maybe (Note event))
noise2_1 = cycleFromSample S.noise2_1__Sample

noise2_2 :: forall event. Cycle (Maybe (Note event))
noise2_2 = cycleFromSample S.noise2_2__Sample

noise2_3 :: forall event. Cycle (Maybe (Note event))
noise2_3 = cycleFromSample S.noise2_3__Sample

noise2_4 :: forall event. Cycle (Maybe (Note event))
noise2_4 = cycleFromSample S.noise2_4__Sample

noise2_5 :: forall event. Cycle (Maybe (Note event))
noise2_5 = cycleFromSample S.noise2_5__Sample

noise2_6 :: forall event. Cycle (Maybe (Note event))
noise2_6 = cycleFromSample S.noise2_6__Sample

noise2_7 :: forall event. Cycle (Maybe (Note event))
noise2_7 = cycleFromSample S.noise2_7__Sample

foo :: forall event. Cycle (Maybe (Note event))
foo = cycleFromSample S.foo_0__Sample

foo_0 :: forall event. Cycle (Maybe (Note event))
foo_0 = cycleFromSample S.foo_0__Sample

foo_1 :: forall event. Cycle (Maybe (Note event))
foo_1 = cycleFromSample S.foo_1__Sample

foo_2 :: forall event. Cycle (Maybe (Note event))
foo_2 = cycleFromSample S.foo_2__Sample

foo_3 :: forall event. Cycle (Maybe (Note event))
foo_3 = cycleFromSample S.foo_3__Sample

foo_4 :: forall event. Cycle (Maybe (Note event))
foo_4 = cycleFromSample S.foo_4__Sample

foo_5 :: forall event. Cycle (Maybe (Note event))
foo_5 = cycleFromSample S.foo_5__Sample

foo_6 :: forall event. Cycle (Maybe (Note event))
foo_6 = cycleFromSample S.foo_6__Sample

foo_7 :: forall event. Cycle (Maybe (Note event))
foo_7 = cycleFromSample S.foo_7__Sample

foo_8 :: forall event. Cycle (Maybe (Note event))
foo_8 = cycleFromSample S.foo_8__Sample

foo_9 :: forall event. Cycle (Maybe (Note event))
foo_9 = cycleFromSample S.foo_9__Sample

foo_10 :: forall event. Cycle (Maybe (Note event))
foo_10 = cycleFromSample S.foo_10__Sample

foo_11 :: forall event. Cycle (Maybe (Note event))
foo_11 = cycleFromSample S.foo_11__Sample

foo_12 :: forall event. Cycle (Maybe (Note event))
foo_12 = cycleFromSample S.foo_12__Sample

foo_13 :: forall event. Cycle (Maybe (Note event))
foo_13 = cycleFromSample S.foo_13__Sample

foo_14 :: forall event. Cycle (Maybe (Note event))
foo_14 = cycleFromSample S.foo_14__Sample

foo_15 :: forall event. Cycle (Maybe (Note event))
foo_15 = cycleFromSample S.foo_15__Sample

foo_16 :: forall event. Cycle (Maybe (Note event))
foo_16 = cycleFromSample S.foo_16__Sample

foo_17 :: forall event. Cycle (Maybe (Note event))
foo_17 = cycleFromSample S.foo_17__Sample

foo_18 :: forall event. Cycle (Maybe (Note event))
foo_18 = cycleFromSample S.foo_18__Sample

foo_19 :: forall event. Cycle (Maybe (Note event))
foo_19 = cycleFromSample S.foo_19__Sample

foo_20 :: forall event. Cycle (Maybe (Note event))
foo_20 = cycleFromSample S.foo_20__Sample

foo_21 :: forall event. Cycle (Maybe (Note event))
foo_21 = cycleFromSample S.foo_21__Sample

foo_22 :: forall event. Cycle (Maybe (Note event))
foo_22 = cycleFromSample S.foo_22__Sample

foo_23 :: forall event. Cycle (Maybe (Note event))
foo_23 = cycleFromSample S.foo_23__Sample

foo_24 :: forall event. Cycle (Maybe (Note event))
foo_24 = cycleFromSample S.foo_24__Sample

foo_25 :: forall event. Cycle (Maybe (Note event))
foo_25 = cycleFromSample S.foo_25__Sample

foo_26 :: forall event. Cycle (Maybe (Note event))
foo_26 = cycleFromSample S.foo_26__Sample

armora :: forall event. Cycle (Maybe (Note event))
armora = cycleFromSample S.armora_0__Sample

armora_0 :: forall event. Cycle (Maybe (Note event))
armora_0 = cycleFromSample S.armora_0__Sample

armora_1 :: forall event. Cycle (Maybe (Note event))
armora_1 = cycleFromSample S.armora_1__Sample

armora_2 :: forall event. Cycle (Maybe (Note event))
armora_2 = cycleFromSample S.armora_2__Sample

armora_3 :: forall event. Cycle (Maybe (Note event))
armora_3 = cycleFromSample S.armora_3__Sample

armora_4 :: forall event. Cycle (Maybe (Note event))
armora_4 = cycleFromSample S.armora_4__Sample

armora_5 :: forall event. Cycle (Maybe (Note event))
armora_5 = cycleFromSample S.armora_5__Sample

armora_6 :: forall event. Cycle (Maybe (Note event))
armora_6 = cycleFromSample S.armora_6__Sample

bend :: forall event. Cycle (Maybe (Note event))
bend = cycleFromSample S.bend_0__Sample

bend_0 :: forall event. Cycle (Maybe (Note event))
bend_0 = cycleFromSample S.bend_0__Sample

bend_1 :: forall event. Cycle (Maybe (Note event))
bend_1 = cycleFromSample S.bend_1__Sample

bend_2 :: forall event. Cycle (Maybe (Note event))
bend_2 = cycleFromSample S.bend_2__Sample

bend_3 :: forall event. Cycle (Maybe (Note event))
bend_3 = cycleFromSample S.bend_3__Sample

newnotes :: forall event. Cycle (Maybe (Note event))
newnotes = cycleFromSample S.newnotes_0__Sample

newnotes_0 :: forall event. Cycle (Maybe (Note event))
newnotes_0 = cycleFromSample S.newnotes_0__Sample

newnotes_1 :: forall event. Cycle (Maybe (Note event))
newnotes_1 = cycleFromSample S.newnotes_1__Sample

newnotes_2 :: forall event. Cycle (Maybe (Note event))
newnotes_2 = cycleFromSample S.newnotes_2__Sample

newnotes_3 :: forall event. Cycle (Maybe (Note event))
newnotes_3 = cycleFromSample S.newnotes_3__Sample

newnotes_4 :: forall event. Cycle (Maybe (Note event))
newnotes_4 = cycleFromSample S.newnotes_4__Sample

newnotes_5 :: forall event. Cycle (Maybe (Note event))
newnotes_5 = cycleFromSample S.newnotes_5__Sample

newnotes_6 :: forall event. Cycle (Maybe (Note event))
newnotes_6 = cycleFromSample S.newnotes_6__Sample

newnotes_7 :: forall event. Cycle (Maybe (Note event))
newnotes_7 = cycleFromSample S.newnotes_7__Sample

newnotes_8 :: forall event. Cycle (Maybe (Note event))
newnotes_8 = cycleFromSample S.newnotes_8__Sample

newnotes_9 :: forall event. Cycle (Maybe (Note event))
newnotes_9 = cycleFromSample S.newnotes_9__Sample

newnotes_10 :: forall event. Cycle (Maybe (Note event))
newnotes_10 = cycleFromSample S.newnotes_10__Sample

newnotes_11 :: forall event. Cycle (Maybe (Note event))
newnotes_11 = cycleFromSample S.newnotes_11__Sample

newnotes_12 :: forall event. Cycle (Maybe (Note event))
newnotes_12 = cycleFromSample S.newnotes_12__Sample

newnotes_13 :: forall event. Cycle (Maybe (Note event))
newnotes_13 = cycleFromSample S.newnotes_13__Sample

newnotes_14 :: forall event. Cycle (Maybe (Note event))
newnotes_14 = cycleFromSample S.newnotes_14__Sample

pebbles :: forall event. Cycle (Maybe (Note event))
pebbles = cycleFromSample S.pebbles_0__Sample

pebbles_0 :: forall event. Cycle (Maybe (Note event))
pebbles_0 = cycleFromSample S.pebbles_0__Sample

mash2 :: forall event. Cycle (Maybe (Note event))
mash2 = cycleFromSample S.mash2_0__Sample

mash2_0 :: forall event. Cycle (Maybe (Note event))
mash2_0 = cycleFromSample S.mash2_0__Sample

mash2_1 :: forall event. Cycle (Maybe (Note event))
mash2_1 = cycleFromSample S.mash2_1__Sample

mash2_2 :: forall event. Cycle (Maybe (Note event))
mash2_2 = cycleFromSample S.mash2_2__Sample

mash2_3 :: forall event. Cycle (Maybe (Note event))
mash2_3 = cycleFromSample S.mash2_3__Sample

diphone2 :: forall event. Cycle (Maybe (Note event))
diphone2 = cycleFromSample S.diphone2_0__Sample

diphone2_0 :: forall event. Cycle (Maybe (Note event))
diphone2_0 = cycleFromSample S.diphone2_0__Sample

diphone2_1 :: forall event. Cycle (Maybe (Note event))
diphone2_1 = cycleFromSample S.diphone2_1__Sample

diphone2_2 :: forall event. Cycle (Maybe (Note event))
diphone2_2 = cycleFromSample S.diphone2_2__Sample

diphone2_3 :: forall event. Cycle (Maybe (Note event))
diphone2_3 = cycleFromSample S.diphone2_3__Sample

diphone2_4 :: forall event. Cycle (Maybe (Note event))
diphone2_4 = cycleFromSample S.diphone2_4__Sample

diphone2_5 :: forall event. Cycle (Maybe (Note event))
diphone2_5 = cycleFromSample S.diphone2_5__Sample

diphone2_6 :: forall event. Cycle (Maybe (Note event))
diphone2_6 = cycleFromSample S.diphone2_6__Sample

diphone2_7 :: forall event. Cycle (Maybe (Note event))
diphone2_7 = cycleFromSample S.diphone2_7__Sample

diphone2_8 :: forall event. Cycle (Maybe (Note event))
diphone2_8 = cycleFromSample S.diphone2_8__Sample

diphone2_9 :: forall event. Cycle (Maybe (Note event))
diphone2_9 = cycleFromSample S.diphone2_9__Sample

diphone2_10 :: forall event. Cycle (Maybe (Note event))
diphone2_10 = cycleFromSample S.diphone2_10__Sample

diphone2_11 :: forall event. Cycle (Maybe (Note event))
diphone2_11 = cycleFromSample S.diphone2_11__Sample

e :: forall event. Cycle (Maybe (Note event))
e = cycleFromSample S.e_0__Sample

e_0 :: forall event. Cycle (Maybe (Note event))
e_0 = cycleFromSample S.e_0__Sample

e_1 :: forall event. Cycle (Maybe (Note event))
e_1 = cycleFromSample S.e_1__Sample

e_2 :: forall event. Cycle (Maybe (Note event))
e_2 = cycleFromSample S.e_2__Sample

e_3 :: forall event. Cycle (Maybe (Note event))
e_3 = cycleFromSample S.e_3__Sample

e_4 :: forall event. Cycle (Maybe (Note event))
e_4 = cycleFromSample S.e_4__Sample

e_5 :: forall event. Cycle (Maybe (Note event))
e_5 = cycleFromSample S.e_5__Sample

e_6 :: forall event. Cycle (Maybe (Note event))
e_6 = cycleFromSample S.e_6__Sample

e_7 :: forall event. Cycle (Maybe (Note event))
e_7 = cycleFromSample S.e_7__Sample

bubble :: forall event. Cycle (Maybe (Note event))
bubble = cycleFromSample S.bubble_0__Sample

bubble_0 :: forall event. Cycle (Maybe (Note event))
bubble_0 = cycleFromSample S.bubble_0__Sample

bubble_1 :: forall event. Cycle (Maybe (Note event))
bubble_1 = cycleFromSample S.bubble_1__Sample

bubble_2 :: forall event. Cycle (Maybe (Note event))
bubble_2 = cycleFromSample S.bubble_2__Sample

bubble_3 :: forall event. Cycle (Maybe (Note event))
bubble_3 = cycleFromSample S.bubble_3__Sample

bubble_4 :: forall event. Cycle (Maybe (Note event))
bubble_4 = cycleFromSample S.bubble_4__Sample

bubble_5 :: forall event. Cycle (Maybe (Note event))
bubble_5 = cycleFromSample S.bubble_5__Sample

bubble_6 :: forall event. Cycle (Maybe (Note event))
bubble_6 = cycleFromSample S.bubble_6__Sample

bubble_7 :: forall event. Cycle (Maybe (Note event))
bubble_7 = cycleFromSample S.bubble_7__Sample

insect :: forall event. Cycle (Maybe (Note event))
insect = cycleFromSample S.insect_0__Sample

insect_0 :: forall event. Cycle (Maybe (Note event))
insect_0 = cycleFromSample S.insect_0__Sample

insect_1 :: forall event. Cycle (Maybe (Note event))
insect_1 = cycleFromSample S.insect_1__Sample

insect_2 :: forall event. Cycle (Maybe (Note event))
insect_2 = cycleFromSample S.insect_2__Sample

ade :: forall event. Cycle (Maybe (Note event))
ade = cycleFromSample S.ade_0__Sample

ade_0 :: forall event. Cycle (Maybe (Note event))
ade_0 = cycleFromSample S.ade_0__Sample

ade_1 :: forall event. Cycle (Maybe (Note event))
ade_1 = cycleFromSample S.ade_1__Sample

ade_2 :: forall event. Cycle (Maybe (Note event))
ade_2 = cycleFromSample S.ade_2__Sample

ade_3 :: forall event. Cycle (Maybe (Note event))
ade_3 = cycleFromSample S.ade_3__Sample

ade_4 :: forall event. Cycle (Maybe (Note event))
ade_4 = cycleFromSample S.ade_4__Sample

ade_5 :: forall event. Cycle (Maybe (Note event))
ade_5 = cycleFromSample S.ade_5__Sample

ade_6 :: forall event. Cycle (Maybe (Note event))
ade_6 = cycleFromSample S.ade_6__Sample

ade_7 :: forall event. Cycle (Maybe (Note event))
ade_7 = cycleFromSample S.ade_7__Sample

ade_8 :: forall event. Cycle (Maybe (Note event))
ade_8 = cycleFromSample S.ade_8__Sample

ade_9 :: forall event. Cycle (Maybe (Note event))
ade_9 = cycleFromSample S.ade_9__Sample

glitch :: forall event. Cycle (Maybe (Note event))
glitch = cycleFromSample S.glitch_0__Sample

glitch_0 :: forall event. Cycle (Maybe (Note event))
glitch_0 = cycleFromSample S.glitch_0__Sample

glitch_1 :: forall event. Cycle (Maybe (Note event))
glitch_1 = cycleFromSample S.glitch_1__Sample

glitch_2 :: forall event. Cycle (Maybe (Note event))
glitch_2 = cycleFromSample S.glitch_2__Sample

glitch_3 :: forall event. Cycle (Maybe (Note event))
glitch_3 = cycleFromSample S.glitch_3__Sample

glitch_4 :: forall event. Cycle (Maybe (Note event))
glitch_4 = cycleFromSample S.glitch_4__Sample

glitch_5 :: forall event. Cycle (Maybe (Note event))
glitch_5 = cycleFromSample S.glitch_5__Sample

glitch_6 :: forall event. Cycle (Maybe (Note event))
glitch_6 = cycleFromSample S.glitch_6__Sample

glitch_7 :: forall event. Cycle (Maybe (Note event))
glitch_7 = cycleFromSample S.glitch_7__Sample

haw :: forall event. Cycle (Maybe (Note event))
haw = cycleFromSample S.haw_0__Sample

haw_0 :: forall event. Cycle (Maybe (Note event))
haw_0 = cycleFromSample S.haw_0__Sample

haw_1 :: forall event. Cycle (Maybe (Note event))
haw_1 = cycleFromSample S.haw_1__Sample

haw_2 :: forall event. Cycle (Maybe (Note event))
haw_2 = cycleFromSample S.haw_2__Sample

haw_3 :: forall event. Cycle (Maybe (Note event))
haw_3 = cycleFromSample S.haw_3__Sample

haw_4 :: forall event. Cycle (Maybe (Note event))
haw_4 = cycleFromSample S.haw_4__Sample

haw_5 :: forall event. Cycle (Maybe (Note event))
haw_5 = cycleFromSample S.haw_5__Sample

popkick :: forall event. Cycle (Maybe (Note event))
popkick = cycleFromSample S.popkick_0__Sample

popkick_0 :: forall event. Cycle (Maybe (Note event))
popkick_0 = cycleFromSample S.popkick_0__Sample

popkick_1 :: forall event. Cycle (Maybe (Note event))
popkick_1 = cycleFromSample S.popkick_1__Sample

popkick_2 :: forall event. Cycle (Maybe (Note event))
popkick_2 = cycleFromSample S.popkick_2__Sample

popkick_3 :: forall event. Cycle (Maybe (Note event))
popkick_3 = cycleFromSample S.popkick_3__Sample

popkick_4 :: forall event. Cycle (Maybe (Note event))
popkick_4 = cycleFromSample S.popkick_4__Sample

popkick_5 :: forall event. Cycle (Maybe (Note event))
popkick_5 = cycleFromSample S.popkick_5__Sample

popkick_6 :: forall event. Cycle (Maybe (Note event))
popkick_6 = cycleFromSample S.popkick_6__Sample

popkick_7 :: forall event. Cycle (Maybe (Note event))
popkick_7 = cycleFromSample S.popkick_7__Sample

popkick_8 :: forall event. Cycle (Maybe (Note event))
popkick_8 = cycleFromSample S.popkick_8__Sample

popkick_9 :: forall event. Cycle (Maybe (Note event))
popkick_9 = cycleFromSample S.popkick_9__Sample

breaks157 :: forall event. Cycle (Maybe (Note event))
breaks157 = cycleFromSample S.breaks157_0__Sample

breaks157_0 :: forall event. Cycle (Maybe (Note event))
breaks157_0 = cycleFromSample S.breaks157_0__Sample

gtr :: forall event. Cycle (Maybe (Note event))
gtr = cycleFromSample S.gtr_0__Sample

gtr_0 :: forall event. Cycle (Maybe (Note event))
gtr_0 = cycleFromSample S.gtr_0__Sample

gtr_1 :: forall event. Cycle (Maybe (Note event))
gtr_1 = cycleFromSample S.gtr_1__Sample

gtr_2 :: forall event. Cycle (Maybe (Note event))
gtr_2 = cycleFromSample S.gtr_2__Sample

clubkick :: forall event. Cycle (Maybe (Note event))
clubkick = cycleFromSample S.clubkick_0__Sample

clubkick_0 :: forall event. Cycle (Maybe (Note event))
clubkick_0 = cycleFromSample S.clubkick_0__Sample

clubkick_1 :: forall event. Cycle (Maybe (Note event))
clubkick_1 = cycleFromSample S.clubkick_1__Sample

clubkick_2 :: forall event. Cycle (Maybe (Note event))
clubkick_2 = cycleFromSample S.clubkick_2__Sample

clubkick_3 :: forall event. Cycle (Maybe (Note event))
clubkick_3 = cycleFromSample S.clubkick_3__Sample

clubkick_4 :: forall event. Cycle (Maybe (Note event))
clubkick_4 = cycleFromSample S.clubkick_4__Sample

breaks152 :: forall event. Cycle (Maybe (Note event))
breaks152 = cycleFromSample S.breaks152_0__Sample

breaks152_0 :: forall event. Cycle (Maybe (Note event))
breaks152_0 = cycleFromSample S.breaks152_0__Sample

x_808bd :: forall event. Cycle (Maybe (Note event))
x_808bd = cycleFromSample S.x_808bd_0__Sample

x_808bd_0 :: forall event. Cycle (Maybe (Note event))
x_808bd_0 = cycleFromSample S.x_808bd_0__Sample

x_808bd_1 :: forall event. Cycle (Maybe (Note event))
x_808bd_1 = cycleFromSample S.x_808bd_1__Sample

x_808bd_2 :: forall event. Cycle (Maybe (Note event))
x_808bd_2 = cycleFromSample S.x_808bd_2__Sample

x_808bd_3 :: forall event. Cycle (Maybe (Note event))
x_808bd_3 = cycleFromSample S.x_808bd_3__Sample

x_808bd_4 :: forall event. Cycle (Maybe (Note event))
x_808bd_4 = cycleFromSample S.x_808bd_4__Sample

x_808bd_5 :: forall event. Cycle (Maybe (Note event))
x_808bd_5 = cycleFromSample S.x_808bd_5__Sample

x_808bd_6 :: forall event. Cycle (Maybe (Note event))
x_808bd_6 = cycleFromSample S.x_808bd_6__Sample

x_808bd_7 :: forall event. Cycle (Maybe (Note event))
x_808bd_7 = cycleFromSample S.x_808bd_7__Sample

x_808bd_8 :: forall event. Cycle (Maybe (Note event))
x_808bd_8 = cycleFromSample S.x_808bd_8__Sample

x_808bd_9 :: forall event. Cycle (Maybe (Note event))
x_808bd_9 = cycleFromSample S.x_808bd_9__Sample

x_808bd_10 :: forall event. Cycle (Maybe (Note event))
x_808bd_10 = cycleFromSample S.x_808bd_10__Sample

x_808bd_11 :: forall event. Cycle (Maybe (Note event))
x_808bd_11 = cycleFromSample S.x_808bd_11__Sample

x_808bd_12 :: forall event. Cycle (Maybe (Note event))
x_808bd_12 = cycleFromSample S.x_808bd_12__Sample

x_808bd_13 :: forall event. Cycle (Maybe (Note event))
x_808bd_13 = cycleFromSample S.x_808bd_13__Sample

x_808bd_14 :: forall event. Cycle (Maybe (Note event))
x_808bd_14 = cycleFromSample S.x_808bd_14__Sample

x_808bd_15 :: forall event. Cycle (Maybe (Note event))
x_808bd_15 = cycleFromSample S.x_808bd_15__Sample

x_808bd_16 :: forall event. Cycle (Maybe (Note event))
x_808bd_16 = cycleFromSample S.x_808bd_16__Sample

x_808bd_17 :: forall event. Cycle (Maybe (Note event))
x_808bd_17 = cycleFromSample S.x_808bd_17__Sample

x_808bd_18 :: forall event. Cycle (Maybe (Note event))
x_808bd_18 = cycleFromSample S.x_808bd_18__Sample

x_808bd_19 :: forall event. Cycle (Maybe (Note event))
x_808bd_19 = cycleFromSample S.x_808bd_19__Sample

x_808bd_20 :: forall event. Cycle (Maybe (Note event))
x_808bd_20 = cycleFromSample S.x_808bd_20__Sample

x_808bd_21 :: forall event. Cycle (Maybe (Note event))
x_808bd_21 = cycleFromSample S.x_808bd_21__Sample

x_808bd_22 :: forall event. Cycle (Maybe (Note event))
x_808bd_22 = cycleFromSample S.x_808bd_22__Sample

x_808bd_23 :: forall event. Cycle (Maybe (Note event))
x_808bd_23 = cycleFromSample S.x_808bd_23__Sample

x_808bd_24 :: forall event. Cycle (Maybe (Note event))
x_808bd_24 = cycleFromSample S.x_808bd_24__Sample

miniyeah :: forall event. Cycle (Maybe (Note event))
miniyeah = cycleFromSample S.miniyeah_0__Sample

miniyeah_0 :: forall event. Cycle (Maybe (Note event))
miniyeah_0 = cycleFromSample S.miniyeah_0__Sample

miniyeah_1 :: forall event. Cycle (Maybe (Note event))
miniyeah_1 = cycleFromSample S.miniyeah_1__Sample

miniyeah_2 :: forall event. Cycle (Maybe (Note event))
miniyeah_2 = cycleFromSample S.miniyeah_2__Sample

miniyeah_3 :: forall event. Cycle (Maybe (Note event))
miniyeah_3 = cycleFromSample S.miniyeah_3__Sample

iff :: forall event. Cycle (Maybe (Note event))
iff = cycleFromSample S.if_0__Sample

if_0 :: forall event. Cycle (Maybe (Note event))
if_0 = cycleFromSample S.if_0__Sample

if_1 :: forall event. Cycle (Maybe (Note event))
if_1 = cycleFromSample S.if_1__Sample

if_2 :: forall event. Cycle (Maybe (Note event))
if_2 = cycleFromSample S.if_2__Sample

if_3 :: forall event. Cycle (Maybe (Note event))
if_3 = cycleFromSample S.if_3__Sample

if_4 :: forall event. Cycle (Maybe (Note event))
if_4 = cycleFromSample S.if_4__Sample

x_808oh :: forall event. Cycle (Maybe (Note event))
x_808oh = cycleFromSample S.x_808oh_0__Sample

x_808oh_0 :: forall event. Cycle (Maybe (Note event))
x_808oh_0 = cycleFromSample S.x_808oh_0__Sample

x_808oh_1 :: forall event. Cycle (Maybe (Note event))
x_808oh_1 = cycleFromSample S.x_808oh_1__Sample

x_808oh_2 :: forall event. Cycle (Maybe (Note event))
x_808oh_2 = cycleFromSample S.x_808oh_2__Sample

x_808oh_3 :: forall event. Cycle (Maybe (Note event))
x_808oh_3 = cycleFromSample S.x_808oh_3__Sample

x_808oh_4 :: forall event. Cycle (Maybe (Note event))
x_808oh_4 = cycleFromSample S.x_808oh_4__Sample

house :: forall event. Cycle (Maybe (Note event))
house = cycleFromSample S.house_0__Sample

house_0 :: forall event. Cycle (Maybe (Note event))
house_0 = cycleFromSample S.house_0__Sample

house_1 :: forall event. Cycle (Maybe (Note event))
house_1 = cycleFromSample S.house_1__Sample

house_2 :: forall event. Cycle (Maybe (Note event))
house_2 = cycleFromSample S.house_2__Sample

house_3 :: forall event. Cycle (Maybe (Note event))
house_3 = cycleFromSample S.house_3__Sample

house_4 :: forall event. Cycle (Maybe (Note event))
house_4 = cycleFromSample S.house_4__Sample

house_5 :: forall event. Cycle (Maybe (Note event))
house_5 = cycleFromSample S.house_5__Sample

house_6 :: forall event. Cycle (Maybe (Note event))
house_6 = cycleFromSample S.house_6__Sample

house_7 :: forall event. Cycle (Maybe (Note event))
house_7 = cycleFromSample S.house_7__Sample

dr :: forall event. Cycle (Maybe (Note event))
dr = cycleFromSample S.dr_0__Sample

dr_0 :: forall event. Cycle (Maybe (Note event))
dr_0 = cycleFromSample S.dr_0__Sample

dr_1 :: forall event. Cycle (Maybe (Note event))
dr_1 = cycleFromSample S.dr_1__Sample

dr_2 :: forall event. Cycle (Maybe (Note event))
dr_2 = cycleFromSample S.dr_2__Sample

dr_3 :: forall event. Cycle (Maybe (Note event))
dr_3 = cycleFromSample S.dr_3__Sample

dr_4 :: forall event. Cycle (Maybe (Note event))
dr_4 = cycleFromSample S.dr_4__Sample

dr_5 :: forall event. Cycle (Maybe (Note event))
dr_5 = cycleFromSample S.dr_5__Sample

dr_6 :: forall event. Cycle (Maybe (Note event))
dr_6 = cycleFromSample S.dr_6__Sample

dr_7 :: forall event. Cycle (Maybe (Note event))
dr_7 = cycleFromSample S.dr_7__Sample

dr_8 :: forall event. Cycle (Maybe (Note event))
dr_8 = cycleFromSample S.dr_8__Sample

dr_9 :: forall event. Cycle (Maybe (Note event))
dr_9 = cycleFromSample S.dr_9__Sample

dr_10 :: forall event. Cycle (Maybe (Note event))
dr_10 = cycleFromSample S.dr_10__Sample

dr_11 :: forall event. Cycle (Maybe (Note event))
dr_11 = cycleFromSample S.dr_11__Sample

dr_12 :: forall event. Cycle (Maybe (Note event))
dr_12 = cycleFromSample S.dr_12__Sample

dr_13 :: forall event. Cycle (Maybe (Note event))
dr_13 = cycleFromSample S.dr_13__Sample

dr_14 :: forall event. Cycle (Maybe (Note event))
dr_14 = cycleFromSample S.dr_14__Sample

dr_15 :: forall event. Cycle (Maybe (Note event))
dr_15 = cycleFromSample S.dr_15__Sample

dr_16 :: forall event. Cycle (Maybe (Note event))
dr_16 = cycleFromSample S.dr_16__Sample

dr_17 :: forall event. Cycle (Maybe (Note event))
dr_17 = cycleFromSample S.dr_17__Sample

dr_18 :: forall event. Cycle (Maybe (Note event))
dr_18 = cycleFromSample S.dr_18__Sample

dr_19 :: forall event. Cycle (Maybe (Note event))
dr_19 = cycleFromSample S.dr_19__Sample

dr_20 :: forall event. Cycle (Maybe (Note event))
dr_20 = cycleFromSample S.dr_20__Sample

dr_21 :: forall event. Cycle (Maybe (Note event))
dr_21 = cycleFromSample S.dr_21__Sample

dr_22 :: forall event. Cycle (Maybe (Note event))
dr_22 = cycleFromSample S.dr_22__Sample

dr_23 :: forall event. Cycle (Maybe (Note event))
dr_23 = cycleFromSample S.dr_23__Sample

dr_24 :: forall event. Cycle (Maybe (Note event))
dr_24 = cycleFromSample S.dr_24__Sample

dr_25 :: forall event. Cycle (Maybe (Note event))
dr_25 = cycleFromSample S.dr_25__Sample

dr_26 :: forall event. Cycle (Maybe (Note event))
dr_26 = cycleFromSample S.dr_26__Sample

dr_27 :: forall event. Cycle (Maybe (Note event))
dr_27 = cycleFromSample S.dr_27__Sample

dr_28 :: forall event. Cycle (Maybe (Note event))
dr_28 = cycleFromSample S.dr_28__Sample

dr_29 :: forall event. Cycle (Maybe (Note event))
dr_29 = cycleFromSample S.dr_29__Sample

dr_30 :: forall event. Cycle (Maybe (Note event))
dr_30 = cycleFromSample S.dr_30__Sample

dr_31 :: forall event. Cycle (Maybe (Note event))
dr_31 = cycleFromSample S.dr_31__Sample

dr_32 :: forall event. Cycle (Maybe (Note event))
dr_32 = cycleFromSample S.dr_32__Sample

dr_33 :: forall event. Cycle (Maybe (Note event))
dr_33 = cycleFromSample S.dr_33__Sample

dr_34 :: forall event. Cycle (Maybe (Note event))
dr_34 = cycleFromSample S.dr_34__Sample

dr_35 :: forall event. Cycle (Maybe (Note event))
dr_35 = cycleFromSample S.dr_35__Sample

dr_36 :: forall event. Cycle (Maybe (Note event))
dr_36 = cycleFromSample S.dr_36__Sample

dr_37 :: forall event. Cycle (Maybe (Note event))
dr_37 = cycleFromSample S.dr_37__Sample

dr_38 :: forall event. Cycle (Maybe (Note event))
dr_38 = cycleFromSample S.dr_38__Sample

dr_39 :: forall event. Cycle (Maybe (Note event))
dr_39 = cycleFromSample S.dr_39__Sample

dr_40 :: forall event. Cycle (Maybe (Note event))
dr_40 = cycleFromSample S.dr_40__Sample

dr_41 :: forall event. Cycle (Maybe (Note event))
dr_41 = cycleFromSample S.dr_41__Sample

dr55 :: forall event. Cycle (Maybe (Note event))
dr55 = cycleFromSample S.dr55_0__Sample

dr55_0 :: forall event. Cycle (Maybe (Note event))
dr55_0 = cycleFromSample S.dr55_0__Sample

dr55_1 :: forall event. Cycle (Maybe (Note event))
dr55_1 = cycleFromSample S.dr55_1__Sample

dr55_2 :: forall event. Cycle (Maybe (Note event))
dr55_2 = cycleFromSample S.dr55_2__Sample

dr55_3 :: forall event. Cycle (Maybe (Note event))
dr55_3 = cycleFromSample S.dr55_3__Sample

bass :: forall event. Cycle (Maybe (Note event))
bass = cycleFromSample S.bass_0__Sample

bass_0 :: forall event. Cycle (Maybe (Note event))
bass_0 = cycleFromSample S.bass_0__Sample

bass_1 :: forall event. Cycle (Maybe (Note event))
bass_1 = cycleFromSample S.bass_1__Sample

bass_2 :: forall event. Cycle (Maybe (Note event))
bass_2 = cycleFromSample S.bass_2__Sample

bass_3 :: forall event. Cycle (Maybe (Note event))
bass_3 = cycleFromSample S.bass_3__Sample

ho :: forall event. Cycle (Maybe (Note event))
ho = cycleFromSample S.ho_0__Sample

ho_0 :: forall event. Cycle (Maybe (Note event))
ho_0 = cycleFromSample S.ho_0__Sample

ho_1 :: forall event. Cycle (Maybe (Note event))
ho_1 = cycleFromSample S.ho_1__Sample

ho_2 :: forall event. Cycle (Maybe (Note event))
ho_2 = cycleFromSample S.ho_2__Sample

ho_3 :: forall event. Cycle (Maybe (Note event))
ho_3 = cycleFromSample S.ho_3__Sample

ho_4 :: forall event. Cycle (Maybe (Note event))
ho_4 = cycleFromSample S.ho_4__Sample

ho_5 :: forall event. Cycle (Maybe (Note event))
ho_5 = cycleFromSample S.ho_5__Sample

hardkick :: forall event. Cycle (Maybe (Note event))
hardkick = cycleFromSample S.hardkick_0__Sample

hardkick_0 :: forall event. Cycle (Maybe (Note event))
hardkick_0 = cycleFromSample S.hardkick_0__Sample

hardkick_1 :: forall event. Cycle (Maybe (Note event))
hardkick_1 = cycleFromSample S.hardkick_1__Sample

hardkick_2 :: forall event. Cycle (Maybe (Note event))
hardkick_2 = cycleFromSample S.hardkick_2__Sample

hardkick_3 :: forall event. Cycle (Maybe (Note event))
hardkick_3 = cycleFromSample S.hardkick_3__Sample

hardkick_4 :: forall event. Cycle (Maybe (Note event))
hardkick_4 = cycleFromSample S.hardkick_4__Sample

hardkick_5 :: forall event. Cycle (Maybe (Note event))
hardkick_5 = cycleFromSample S.hardkick_5__Sample

x_808hc :: forall event. Cycle (Maybe (Note event))
x_808hc = cycleFromSample S.x_808hc_0__Sample

x_808hc_0 :: forall event. Cycle (Maybe (Note event))
x_808hc_0 = cycleFromSample S.x_808hc_0__Sample

x_808hc_1 :: forall event. Cycle (Maybe (Note event))
x_808hc_1 = cycleFromSample S.x_808hc_1__Sample

x_808hc_2 :: forall event. Cycle (Maybe (Note event))
x_808hc_2 = cycleFromSample S.x_808hc_2__Sample

x_808hc_3 :: forall event. Cycle (Maybe (Note event))
x_808hc_3 = cycleFromSample S.x_808hc_3__Sample

x_808hc_4 :: forall event. Cycle (Maybe (Note event))
x_808hc_4 = cycleFromSample S.x_808hc_4__Sample

hit :: forall event. Cycle (Maybe (Note event))
hit = cycleFromSample S.hit_0__Sample

hit_0 :: forall event. Cycle (Maybe (Note event))
hit_0 = cycleFromSample S.hit_0__Sample

hit_1 :: forall event. Cycle (Maybe (Note event))
hit_1 = cycleFromSample S.hit_1__Sample

hit_2 :: forall event. Cycle (Maybe (Note event))
hit_2 = cycleFromSample S.hit_2__Sample

hit_3 :: forall event. Cycle (Maybe (Note event))
hit_3 = cycleFromSample S.hit_3__Sample

hit_4 :: forall event. Cycle (Maybe (Note event))
hit_4 = cycleFromSample S.hit_4__Sample

hit_5 :: forall event. Cycle (Maybe (Note event))
hit_5 = cycleFromSample S.hit_5__Sample

breaks165 :: forall event. Cycle (Maybe (Note event))
breaks165 = cycleFromSample S.breaks165_0__Sample

breaks165_0 :: forall event. Cycle (Maybe (Note event))
breaks165_0 = cycleFromSample S.breaks165_0__Sample

dr2 :: forall event. Cycle (Maybe (Note event))
dr2 = cycleFromSample S.dr2_0__Sample

dr2_0 :: forall event. Cycle (Maybe (Note event))
dr2_0 = cycleFromSample S.dr2_0__Sample

dr2_1 :: forall event. Cycle (Maybe (Note event))
dr2_1 = cycleFromSample S.dr2_1__Sample

dr2_2 :: forall event. Cycle (Maybe (Note event))
dr2_2 = cycleFromSample S.dr2_2__Sample

dr2_3 :: forall event. Cycle (Maybe (Note event))
dr2_3 = cycleFromSample S.dr2_3__Sample

dr2_4 :: forall event. Cycle (Maybe (Note event))
dr2_4 = cycleFromSample S.dr2_4__Sample

dr2_5 :: forall event. Cycle (Maybe (Note event))
dr2_5 = cycleFromSample S.dr2_5__Sample

tabla :: forall event. Cycle (Maybe (Note event))
tabla = cycleFromSample S.tabla_0__Sample

tabla_0 :: forall event. Cycle (Maybe (Note event))
tabla_0 = cycleFromSample S.tabla_0__Sample

tabla_1 :: forall event. Cycle (Maybe (Note event))
tabla_1 = cycleFromSample S.tabla_1__Sample

tabla_2 :: forall event. Cycle (Maybe (Note event))
tabla_2 = cycleFromSample S.tabla_2__Sample

tabla_3 :: forall event. Cycle (Maybe (Note event))
tabla_3 = cycleFromSample S.tabla_3__Sample

tabla_4 :: forall event. Cycle (Maybe (Note event))
tabla_4 = cycleFromSample S.tabla_4__Sample

tabla_5 :: forall event. Cycle (Maybe (Note event))
tabla_5 = cycleFromSample S.tabla_5__Sample

tabla_6 :: forall event. Cycle (Maybe (Note event))
tabla_6 = cycleFromSample S.tabla_6__Sample

tabla_7 :: forall event. Cycle (Maybe (Note event))
tabla_7 = cycleFromSample S.tabla_7__Sample

tabla_8 :: forall event. Cycle (Maybe (Note event))
tabla_8 = cycleFromSample S.tabla_8__Sample

tabla_9 :: forall event. Cycle (Maybe (Note event))
tabla_9 = cycleFromSample S.tabla_9__Sample

tabla_10 :: forall event. Cycle (Maybe (Note event))
tabla_10 = cycleFromSample S.tabla_10__Sample

tabla_11 :: forall event. Cycle (Maybe (Note event))
tabla_11 = cycleFromSample S.tabla_11__Sample

tabla_12 :: forall event. Cycle (Maybe (Note event))
tabla_12 = cycleFromSample S.tabla_12__Sample

tabla_13 :: forall event. Cycle (Maybe (Note event))
tabla_13 = cycleFromSample S.tabla_13__Sample

tabla_14 :: forall event. Cycle (Maybe (Note event))
tabla_14 = cycleFromSample S.tabla_14__Sample

tabla_15 :: forall event. Cycle (Maybe (Note event))
tabla_15 = cycleFromSample S.tabla_15__Sample

tabla_16 :: forall event. Cycle (Maybe (Note event))
tabla_16 = cycleFromSample S.tabla_16__Sample

tabla_17 :: forall event. Cycle (Maybe (Note event))
tabla_17 = cycleFromSample S.tabla_17__Sample

tabla_18 :: forall event. Cycle (Maybe (Note event))
tabla_18 = cycleFromSample S.tabla_18__Sample

tabla_19 :: forall event. Cycle (Maybe (Note event))
tabla_19 = cycleFromSample S.tabla_19__Sample

tabla_20 :: forall event. Cycle (Maybe (Note event))
tabla_20 = cycleFromSample S.tabla_20__Sample

tabla_21 :: forall event. Cycle (Maybe (Note event))
tabla_21 = cycleFromSample S.tabla_21__Sample

tabla_22 :: forall event. Cycle (Maybe (Note event))
tabla_22 = cycleFromSample S.tabla_22__Sample

tabla_23 :: forall event. Cycle (Maybe (Note event))
tabla_23 = cycleFromSample S.tabla_23__Sample

tabla_24 :: forall event. Cycle (Maybe (Note event))
tabla_24 = cycleFromSample S.tabla_24__Sample

tabla_25 :: forall event. Cycle (Maybe (Note event))
tabla_25 = cycleFromSample S.tabla_25__Sample

dork2 :: forall event. Cycle (Maybe (Note event))
dork2 = cycleFromSample S.dork2_0__Sample

dork2_0 :: forall event. Cycle (Maybe (Note event))
dork2_0 = cycleFromSample S.dork2_0__Sample

dork2_1 :: forall event. Cycle (Maybe (Note event))
dork2_1 = cycleFromSample S.dork2_1__Sample

dork2_2 :: forall event. Cycle (Maybe (Note event))
dork2_2 = cycleFromSample S.dork2_2__Sample

dork2_3 :: forall event. Cycle (Maybe (Note event))
dork2_3 = cycleFromSample S.dork2_3__Sample

hc :: forall event. Cycle (Maybe (Note event))
hc = cycleFromSample S.hc_0__Sample

hc_0 :: forall event. Cycle (Maybe (Note event))
hc_0 = cycleFromSample S.hc_0__Sample

hc_1 :: forall event. Cycle (Maybe (Note event))
hc_1 = cycleFromSample S.hc_1__Sample

hc_2 :: forall event. Cycle (Maybe (Note event))
hc_2 = cycleFromSample S.hc_2__Sample

hc_3 :: forall event. Cycle (Maybe (Note event))
hc_3 = cycleFromSample S.hc_3__Sample

hc_4 :: forall event. Cycle (Maybe (Note event))
hc_4 = cycleFromSample S.hc_4__Sample

hc_5 :: forall event. Cycle (Maybe (Note event))
hc_5 = cycleFromSample S.hc_5__Sample

bassfoo :: forall event. Cycle (Maybe (Note event))
bassfoo = cycleFromSample S.bassfoo_0__Sample

bassfoo_0 :: forall event. Cycle (Maybe (Note event))
bassfoo_0 = cycleFromSample S.bassfoo_0__Sample

bassfoo_1 :: forall event. Cycle (Maybe (Note event))
bassfoo_1 = cycleFromSample S.bassfoo_1__Sample

bassfoo_2 :: forall event. Cycle (Maybe (Note event))
bassfoo_2 = cycleFromSample S.bassfoo_2__Sample

seawolf :: forall event. Cycle (Maybe (Note event))
seawolf = cycleFromSample S.seawolf_0__Sample

seawolf_0 :: forall event. Cycle (Maybe (Note event))
seawolf_0 = cycleFromSample S.seawolf_0__Sample

seawolf_1 :: forall event. Cycle (Maybe (Note event))
seawolf_1 = cycleFromSample S.seawolf_1__Sample

seawolf_2 :: forall event. Cycle (Maybe (Note event))
seawolf_2 = cycleFromSample S.seawolf_2__Sample

cp :: forall event. Cycle (Maybe (Note event))
cp = cycleFromSample S.cp_0__Sample

cp_0 :: forall event. Cycle (Maybe (Note event))
cp_0 = cycleFromSample S.cp_0__Sample

cp_1 :: forall event. Cycle (Maybe (Note event))
cp_1 = cycleFromSample S.cp_1__Sample

jazz :: forall event. Cycle (Maybe (Note event))
jazz = cycleFromSample S.jazz_0__Sample

jazz_0 :: forall event. Cycle (Maybe (Note event))
jazz_0 = cycleFromSample S.jazz_0__Sample

jazz_1 :: forall event. Cycle (Maybe (Note event))
jazz_1 = cycleFromSample S.jazz_1__Sample

jazz_2 :: forall event. Cycle (Maybe (Note event))
jazz_2 = cycleFromSample S.jazz_2__Sample

jazz_3 :: forall event. Cycle (Maybe (Note event))
jazz_3 = cycleFromSample S.jazz_3__Sample

jazz_4 :: forall event. Cycle (Maybe (Note event))
jazz_4 = cycleFromSample S.jazz_4__Sample

jazz_5 :: forall event. Cycle (Maybe (Note event))
jazz_5 = cycleFromSample S.jazz_5__Sample

jazz_6 :: forall event. Cycle (Maybe (Note event))
jazz_6 = cycleFromSample S.jazz_6__Sample

jazz_7 :: forall event. Cycle (Maybe (Note event))
jazz_7 = cycleFromSample S.jazz_7__Sample

juno :: forall event. Cycle (Maybe (Note event))
juno = cycleFromSample S.juno_0__Sample

juno_0 :: forall event. Cycle (Maybe (Note event))
juno_0 = cycleFromSample S.juno_0__Sample

juno_1 :: forall event. Cycle (Maybe (Note event))
juno_1 = cycleFromSample S.juno_1__Sample

juno_2 :: forall event. Cycle (Maybe (Note event))
juno_2 = cycleFromSample S.juno_2__Sample

juno_3 :: forall event. Cycle (Maybe (Note event))
juno_3 = cycleFromSample S.juno_3__Sample

juno_4 :: forall event. Cycle (Maybe (Note event))
juno_4 = cycleFromSample S.juno_4__Sample

juno_5 :: forall event. Cycle (Maybe (Note event))
juno_5 = cycleFromSample S.juno_5__Sample

juno_6 :: forall event. Cycle (Maybe (Note event))
juno_6 = cycleFromSample S.juno_6__Sample

juno_7 :: forall event. Cycle (Maybe (Note event))
juno_7 = cycleFromSample S.juno_7__Sample

juno_8 :: forall event. Cycle (Maybe (Note event))
juno_8 = cycleFromSample S.juno_8__Sample

juno_9 :: forall event. Cycle (Maybe (Note event))
juno_9 = cycleFromSample S.juno_9__Sample

juno_10 :: forall event. Cycle (Maybe (Note event))
juno_10 = cycleFromSample S.juno_10__Sample

juno_11 :: forall event. Cycle (Maybe (Note event))
juno_11 = cycleFromSample S.juno_11__Sample

birds :: forall event. Cycle (Maybe (Note event))
birds = cycleFromSample S.birds_0__Sample

birds_0 :: forall event. Cycle (Maybe (Note event))
birds_0 = cycleFromSample S.birds_0__Sample

birds_1 :: forall event. Cycle (Maybe (Note event))
birds_1 = cycleFromSample S.birds_1__Sample

birds_2 :: forall event. Cycle (Maybe (Note event))
birds_2 = cycleFromSample S.birds_2__Sample

birds_3 :: forall event. Cycle (Maybe (Note event))
birds_3 = cycleFromSample S.birds_3__Sample

birds_4 :: forall event. Cycle (Maybe (Note event))
birds_4 = cycleFromSample S.birds_4__Sample

birds_5 :: forall event. Cycle (Maybe (Note event))
birds_5 = cycleFromSample S.birds_5__Sample

birds_6 :: forall event. Cycle (Maybe (Note event))
birds_6 = cycleFromSample S.birds_6__Sample

birds_7 :: forall event. Cycle (Maybe (Note event))
birds_7 = cycleFromSample S.birds_7__Sample

birds_8 :: forall event. Cycle (Maybe (Note event))
birds_8 = cycleFromSample S.birds_8__Sample

birds_9 :: forall event. Cycle (Maybe (Note event))
birds_9 = cycleFromSample S.birds_9__Sample

glasstap :: forall event. Cycle (Maybe (Note event))
glasstap = cycleFromSample S.glasstap_0__Sample

glasstap_0 :: forall event. Cycle (Maybe (Note event))
glasstap_0 = cycleFromSample S.glasstap_0__Sample

glasstap_1 :: forall event. Cycle (Maybe (Note event))
glasstap_1 = cycleFromSample S.glasstap_1__Sample

glasstap_2 :: forall event. Cycle (Maybe (Note event))
glasstap_2 = cycleFromSample S.glasstap_2__Sample

bass1 :: forall event. Cycle (Maybe (Note event))
bass1 = cycleFromSample S.bass1_0__Sample

bass1_0 :: forall event. Cycle (Maybe (Note event))
bass1_0 = cycleFromSample S.bass1_0__Sample

bass1_1 :: forall event. Cycle (Maybe (Note event))
bass1_1 = cycleFromSample S.bass1_1__Sample

bass1_2 :: forall event. Cycle (Maybe (Note event))
bass1_2 = cycleFromSample S.bass1_2__Sample

bass1_3 :: forall event. Cycle (Maybe (Note event))
bass1_3 = cycleFromSample S.bass1_3__Sample

bass1_4 :: forall event. Cycle (Maybe (Note event))
bass1_4 = cycleFromSample S.bass1_4__Sample

bass1_5 :: forall event. Cycle (Maybe (Note event))
bass1_5 = cycleFromSample S.bass1_5__Sample

bass1_6 :: forall event. Cycle (Maybe (Note event))
bass1_6 = cycleFromSample S.bass1_6__Sample

bass1_7 :: forall event. Cycle (Maybe (Note event))
bass1_7 = cycleFromSample S.bass1_7__Sample

bass1_8 :: forall event. Cycle (Maybe (Note event))
bass1_8 = cycleFromSample S.bass1_8__Sample

bass1_9 :: forall event. Cycle (Maybe (Note event))
bass1_9 = cycleFromSample S.bass1_9__Sample

bass1_10 :: forall event. Cycle (Maybe (Note event))
bass1_10 = cycleFromSample S.bass1_10__Sample

bass1_11 :: forall event. Cycle (Maybe (Note event))
bass1_11 = cycleFromSample S.bass1_11__Sample

bass1_12 :: forall event. Cycle (Maybe (Note event))
bass1_12 = cycleFromSample S.bass1_12__Sample

bass1_13 :: forall event. Cycle (Maybe (Note event))
bass1_13 = cycleFromSample S.bass1_13__Sample

bass1_14 :: forall event. Cycle (Maybe (Note event))
bass1_14 = cycleFromSample S.bass1_14__Sample

bass1_15 :: forall event. Cycle (Maybe (Note event))
bass1_15 = cycleFromSample S.bass1_15__Sample

bass1_16 :: forall event. Cycle (Maybe (Note event))
bass1_16 = cycleFromSample S.bass1_16__Sample

bass1_17 :: forall event. Cycle (Maybe (Note event))
bass1_17 = cycleFromSample S.bass1_17__Sample

bass1_18 :: forall event. Cycle (Maybe (Note event))
bass1_18 = cycleFromSample S.bass1_18__Sample

bass1_19 :: forall event. Cycle (Maybe (Note event))
bass1_19 = cycleFromSample S.bass1_19__Sample

bass1_20 :: forall event. Cycle (Maybe (Note event))
bass1_20 = cycleFromSample S.bass1_20__Sample

bass1_21 :: forall event. Cycle (Maybe (Note event))
bass1_21 = cycleFromSample S.bass1_21__Sample

bass1_22 :: forall event. Cycle (Maybe (Note event))
bass1_22 = cycleFromSample S.bass1_22__Sample

bass1_23 :: forall event. Cycle (Maybe (Note event))
bass1_23 = cycleFromSample S.bass1_23__Sample

bass1_24 :: forall event. Cycle (Maybe (Note event))
bass1_24 = cycleFromSample S.bass1_24__Sample

bass1_25 :: forall event. Cycle (Maybe (Note event))
bass1_25 = cycleFromSample S.bass1_25__Sample

bass1_26 :: forall event. Cycle (Maybe (Note event))
bass1_26 = cycleFromSample S.bass1_26__Sample

bass1_27 :: forall event. Cycle (Maybe (Note event))
bass1_27 = cycleFromSample S.bass1_27__Sample

bass1_28 :: forall event. Cycle (Maybe (Note event))
bass1_28 = cycleFromSample S.bass1_28__Sample

bass1_29 :: forall event. Cycle (Maybe (Note event))
bass1_29 = cycleFromSample S.bass1_29__Sample

hh27 :: forall event. Cycle (Maybe (Note event))
hh27 = cycleFromSample S.hh27_0__Sample

hh27_0 :: forall event. Cycle (Maybe (Note event))
hh27_0 = cycleFromSample S.hh27_0__Sample

hh27_1 :: forall event. Cycle (Maybe (Note event))
hh27_1 = cycleFromSample S.hh27_1__Sample

hh27_2 :: forall event. Cycle (Maybe (Note event))
hh27_2 = cycleFromSample S.hh27_2__Sample

hh27_3 :: forall event. Cycle (Maybe (Note event))
hh27_3 = cycleFromSample S.hh27_3__Sample

hh27_4 :: forall event. Cycle (Maybe (Note event))
hh27_4 = cycleFromSample S.hh27_4__Sample

hh27_5 :: forall event. Cycle (Maybe (Note event))
hh27_5 = cycleFromSample S.hh27_5__Sample

hh27_6 :: forall event. Cycle (Maybe (Note event))
hh27_6 = cycleFromSample S.hh27_6__Sample

hh27_7 :: forall event. Cycle (Maybe (Note event))
hh27_7 = cycleFromSample S.hh27_7__Sample

hh27_8 :: forall event. Cycle (Maybe (Note event))
hh27_8 = cycleFromSample S.hh27_8__Sample

hh27_9 :: forall event. Cycle (Maybe (Note event))
hh27_9 = cycleFromSample S.hh27_9__Sample

hh27_10 :: forall event. Cycle (Maybe (Note event))
hh27_10 = cycleFromSample S.hh27_10__Sample

hh27_11 :: forall event. Cycle (Maybe (Note event))
hh27_11 = cycleFromSample S.hh27_11__Sample

hh27_12 :: forall event. Cycle (Maybe (Note event))
hh27_12 = cycleFromSample S.hh27_12__Sample

x_808 :: forall event. Cycle (Maybe (Note event))
x_808 = cycleFromSample S.x_808_0__Sample

x_808_0 :: forall event. Cycle (Maybe (Note event))
x_808_0 = cycleFromSample S.x_808_0__Sample

x_808_1 :: forall event. Cycle (Maybe (Note event))
x_808_1 = cycleFromSample S.x_808_1__Sample

x_808_2 :: forall event. Cycle (Maybe (Note event))
x_808_2 = cycleFromSample S.x_808_2__Sample

x_808_3 :: forall event. Cycle (Maybe (Note event))
x_808_3 = cycleFromSample S.x_808_3__Sample

x_808_4 :: forall event. Cycle (Maybe (Note event))
x_808_4 = cycleFromSample S.x_808_4__Sample

x_808_5 :: forall event. Cycle (Maybe (Note event))
x_808_5 = cycleFromSample S.x_808_5__Sample

notes :: forall event. Cycle (Maybe (Note event))
notes = cycleFromSample S.notes_0__Sample

notes_0 :: forall event. Cycle (Maybe (Note event))
notes_0 = cycleFromSample S.notes_0__Sample

notes_1 :: forall event. Cycle (Maybe (Note event))
notes_1 = cycleFromSample S.notes_1__Sample

notes_2 :: forall event. Cycle (Maybe (Note event))
notes_2 = cycleFromSample S.notes_2__Sample

notes_3 :: forall event. Cycle (Maybe (Note event))
notes_3 = cycleFromSample S.notes_3__Sample

notes_4 :: forall event. Cycle (Maybe (Note event))
notes_4 = cycleFromSample S.notes_4__Sample

notes_5 :: forall event. Cycle (Maybe (Note event))
notes_5 = cycleFromSample S.notes_5__Sample

notes_6 :: forall event. Cycle (Maybe (Note event))
notes_6 = cycleFromSample S.notes_6__Sample

notes_7 :: forall event. Cycle (Maybe (Note event))
notes_7 = cycleFromSample S.notes_7__Sample

notes_8 :: forall event. Cycle (Maybe (Note event))
notes_8 = cycleFromSample S.notes_8__Sample

notes_9 :: forall event. Cycle (Maybe (Note event))
notes_9 = cycleFromSample S.notes_9__Sample

notes_10 :: forall event. Cycle (Maybe (Note event))
notes_10 = cycleFromSample S.notes_10__Sample

notes_11 :: forall event. Cycle (Maybe (Note event))
notes_11 = cycleFromSample S.notes_11__Sample

notes_12 :: forall event. Cycle (Maybe (Note event))
notes_12 = cycleFromSample S.notes_12__Sample

notes_13 :: forall event. Cycle (Maybe (Note event))
notes_13 = cycleFromSample S.notes_13__Sample

notes_14 :: forall event. Cycle (Maybe (Note event))
notes_14 = cycleFromSample S.notes_14__Sample

xmas :: forall event. Cycle (Maybe (Note event))
xmas = cycleFromSample S.xmas_0__Sample

xmas_0 :: forall event. Cycle (Maybe (Note event))
xmas_0 = cycleFromSample S.xmas_0__Sample

erk :: forall event. Cycle (Maybe (Note event))
erk = cycleFromSample S.erk_0__Sample

erk_0 :: forall event. Cycle (Maybe (Note event))
erk_0 = cycleFromSample S.erk_0__Sample

x_808mt :: forall event. Cycle (Maybe (Note event))
x_808mt = cycleFromSample S.x_808mt_0__Sample

x_808mt_0 :: forall event. Cycle (Maybe (Note event))
x_808mt_0 = cycleFromSample S.x_808mt_0__Sample

x_808mt_1 :: forall event. Cycle (Maybe (Note event))
x_808mt_1 = cycleFromSample S.x_808mt_1__Sample

x_808mt_2 :: forall event. Cycle (Maybe (Note event))
x_808mt_2 = cycleFromSample S.x_808mt_2__Sample

x_808mt_3 :: forall event. Cycle (Maybe (Note event))
x_808mt_3 = cycleFromSample S.x_808mt_3__Sample

x_808mt_4 :: forall event. Cycle (Maybe (Note event))
x_808mt_4 = cycleFromSample S.x_808mt_4__Sample

lighter :: forall event. Cycle (Maybe (Note event))
lighter = cycleFromSample S.lighter_0__Sample

lighter_0 :: forall event. Cycle (Maybe (Note event))
lighter_0 = cycleFromSample S.lighter_0__Sample

lighter_1 :: forall event. Cycle (Maybe (Note event))
lighter_1 = cycleFromSample S.lighter_1__Sample

lighter_2 :: forall event. Cycle (Maybe (Note event))
lighter_2 = cycleFromSample S.lighter_2__Sample

lighter_3 :: forall event. Cycle (Maybe (Note event))
lighter_3 = cycleFromSample S.lighter_3__Sample

lighter_4 :: forall event. Cycle (Maybe (Note event))
lighter_4 = cycleFromSample S.lighter_4__Sample

lighter_5 :: forall event. Cycle (Maybe (Note event))
lighter_5 = cycleFromSample S.lighter_5__Sample

lighter_6 :: forall event. Cycle (Maybe (Note event))
lighter_6 = cycleFromSample S.lighter_6__Sample

lighter_7 :: forall event. Cycle (Maybe (Note event))
lighter_7 = cycleFromSample S.lighter_7__Sample

lighter_8 :: forall event. Cycle (Maybe (Note event))
lighter_8 = cycleFromSample S.lighter_8__Sample

lighter_9 :: forall event. Cycle (Maybe (Note event))
lighter_9 = cycleFromSample S.lighter_9__Sample

lighter_10 :: forall event. Cycle (Maybe (Note event))
lighter_10 = cycleFromSample S.lighter_10__Sample

lighter_11 :: forall event. Cycle (Maybe (Note event))
lighter_11 = cycleFromSample S.lighter_11__Sample

lighter_12 :: forall event. Cycle (Maybe (Note event))
lighter_12 = cycleFromSample S.lighter_12__Sample

lighter_13 :: forall event. Cycle (Maybe (Note event))
lighter_13 = cycleFromSample S.lighter_13__Sample

lighter_14 :: forall event. Cycle (Maybe (Note event))
lighter_14 = cycleFromSample S.lighter_14__Sample

lighter_15 :: forall event. Cycle (Maybe (Note event))
lighter_15 = cycleFromSample S.lighter_15__Sample

lighter_16 :: forall event. Cycle (Maybe (Note event))
lighter_16 = cycleFromSample S.lighter_16__Sample

lighter_17 :: forall event. Cycle (Maybe (Note event))
lighter_17 = cycleFromSample S.lighter_17__Sample

lighter_18 :: forall event. Cycle (Maybe (Note event))
lighter_18 = cycleFromSample S.lighter_18__Sample

lighter_19 :: forall event. Cycle (Maybe (Note event))
lighter_19 = cycleFromSample S.lighter_19__Sample

lighter_20 :: forall event. Cycle (Maybe (Note event))
lighter_20 = cycleFromSample S.lighter_20__Sample

lighter_21 :: forall event. Cycle (Maybe (Note event))
lighter_21 = cycleFromSample S.lighter_21__Sample

lighter_22 :: forall event. Cycle (Maybe (Note event))
lighter_22 = cycleFromSample S.lighter_22__Sample

lighter_23 :: forall event. Cycle (Maybe (Note event))
lighter_23 = cycleFromSample S.lighter_23__Sample

lighter_24 :: forall event. Cycle (Maybe (Note event))
lighter_24 = cycleFromSample S.lighter_24__Sample

lighter_25 :: forall event. Cycle (Maybe (Note event))
lighter_25 = cycleFromSample S.lighter_25__Sample

lighter_26 :: forall event. Cycle (Maybe (Note event))
lighter_26 = cycleFromSample S.lighter_26__Sample

lighter_27 :: forall event. Cycle (Maybe (Note event))
lighter_27 = cycleFromSample S.lighter_27__Sample

lighter_28 :: forall event. Cycle (Maybe (Note event))
lighter_28 = cycleFromSample S.lighter_28__Sample

lighter_29 :: forall event. Cycle (Maybe (Note event))
lighter_29 = cycleFromSample S.lighter_29__Sample

lighter_30 :: forall event. Cycle (Maybe (Note event))
lighter_30 = cycleFromSample S.lighter_30__Sample

lighter_31 :: forall event. Cycle (Maybe (Note event))
lighter_31 = cycleFromSample S.lighter_31__Sample

lighter_32 :: forall event. Cycle (Maybe (Note event))
lighter_32 = cycleFromSample S.lighter_32__Sample

cb :: forall event. Cycle (Maybe (Note event))
cb = cycleFromSample S.cb_0__Sample

cb_0 :: forall event. Cycle (Maybe (Note event))
cb_0 = cycleFromSample S.cb_0__Sample

subroc3d :: forall event. Cycle (Maybe (Note event))
subroc3d = cycleFromSample S.subroc3d_0__Sample

subroc3d_0 :: forall event. Cycle (Maybe (Note event))
subroc3d_0 = cycleFromSample S.subroc3d_0__Sample

subroc3d_1 :: forall event. Cycle (Maybe (Note event))
subroc3d_1 = cycleFromSample S.subroc3d_1__Sample

subroc3d_2 :: forall event. Cycle (Maybe (Note event))
subroc3d_2 = cycleFromSample S.subroc3d_2__Sample

subroc3d_3 :: forall event. Cycle (Maybe (Note event))
subroc3d_3 = cycleFromSample S.subroc3d_3__Sample

subroc3d_4 :: forall event. Cycle (Maybe (Note event))
subroc3d_4 = cycleFromSample S.subroc3d_4__Sample

subroc3d_5 :: forall event. Cycle (Maybe (Note event))
subroc3d_5 = cycleFromSample S.subroc3d_5__Sample

subroc3d_6 :: forall event. Cycle (Maybe (Note event))
subroc3d_6 = cycleFromSample S.subroc3d_6__Sample

subroc3d_7 :: forall event. Cycle (Maybe (Note event))
subroc3d_7 = cycleFromSample S.subroc3d_7__Sample

subroc3d_8 :: forall event. Cycle (Maybe (Note event))
subroc3d_8 = cycleFromSample S.subroc3d_8__Sample

subroc3d_9 :: forall event. Cycle (Maybe (Note event))
subroc3d_9 = cycleFromSample S.subroc3d_9__Sample

subroc3d_10 :: forall event. Cycle (Maybe (Note event))
subroc3d_10 = cycleFromSample S.subroc3d_10__Sample

ul :: forall event. Cycle (Maybe (Note event))
ul = cycleFromSample S.ul_0__Sample

ul_0 :: forall event. Cycle (Maybe (Note event))
ul_0 = cycleFromSample S.ul_0__Sample

ul_1 :: forall event. Cycle (Maybe (Note event))
ul_1 = cycleFromSample S.ul_1__Sample

ul_2 :: forall event. Cycle (Maybe (Note event))
ul_2 = cycleFromSample S.ul_2__Sample

ul_3 :: forall event. Cycle (Maybe (Note event))
ul_3 = cycleFromSample S.ul_3__Sample

ul_4 :: forall event. Cycle (Maybe (Note event))
ul_4 = cycleFromSample S.ul_4__Sample

ul_5 :: forall event. Cycle (Maybe (Note event))
ul_5 = cycleFromSample S.ul_5__Sample

ul_6 :: forall event. Cycle (Maybe (Note event))
ul_6 = cycleFromSample S.ul_6__Sample

ul_7 :: forall event. Cycle (Maybe (Note event))
ul_7 = cycleFromSample S.ul_7__Sample

ul_8 :: forall event. Cycle (Maybe (Note event))
ul_8 = cycleFromSample S.ul_8__Sample

ul_9 :: forall event. Cycle (Maybe (Note event))
ul_9 = cycleFromSample S.ul_9__Sample

gab :: forall event. Cycle (Maybe (Note event))
gab = cycleFromSample S.gab_0__Sample

gab_0 :: forall event. Cycle (Maybe (Note event))
gab_0 = cycleFromSample S.gab_0__Sample

gab_1 :: forall event. Cycle (Maybe (Note event))
gab_1 = cycleFromSample S.gab_1__Sample

gab_2 :: forall event. Cycle (Maybe (Note event))
gab_2 = cycleFromSample S.gab_2__Sample

gab_3 :: forall event. Cycle (Maybe (Note event))
gab_3 = cycleFromSample S.gab_3__Sample

gab_4 :: forall event. Cycle (Maybe (Note event))
gab_4 = cycleFromSample S.gab_4__Sample

gab_5 :: forall event. Cycle (Maybe (Note event))
gab_5 = cycleFromSample S.gab_5__Sample

gab_6 :: forall event. Cycle (Maybe (Note event))
gab_6 = cycleFromSample S.gab_6__Sample

gab_7 :: forall event. Cycle (Maybe (Note event))
gab_7 = cycleFromSample S.gab_7__Sample

gab_8 :: forall event. Cycle (Maybe (Note event))
gab_8 = cycleFromSample S.gab_8__Sample

gab_9 :: forall event. Cycle (Maybe (Note event))
gab_9 = cycleFromSample S.gab_9__Sample

monsterb :: forall event. Cycle (Maybe (Note event))
monsterb = cycleFromSample S.monsterb_0__Sample

monsterb_0 :: forall event. Cycle (Maybe (Note event))
monsterb_0 = cycleFromSample S.monsterb_0__Sample

monsterb_1 :: forall event. Cycle (Maybe (Note event))
monsterb_1 = cycleFromSample S.monsterb_1__Sample

monsterb_2 :: forall event. Cycle (Maybe (Note event))
monsterb_2 = cycleFromSample S.monsterb_2__Sample

monsterb_3 :: forall event. Cycle (Maybe (Note event))
monsterb_3 = cycleFromSample S.monsterb_3__Sample

monsterb_4 :: forall event. Cycle (Maybe (Note event))
monsterb_4 = cycleFromSample S.monsterb_4__Sample

monsterb_5 :: forall event. Cycle (Maybe (Note event))
monsterb_5 = cycleFromSample S.monsterb_5__Sample

diphone :: forall event. Cycle (Maybe (Note event))
diphone = cycleFromSample S.diphone_0__Sample

diphone_0 :: forall event. Cycle (Maybe (Note event))
diphone_0 = cycleFromSample S.diphone_0__Sample

diphone_1 :: forall event. Cycle (Maybe (Note event))
diphone_1 = cycleFromSample S.diphone_1__Sample

diphone_2 :: forall event. Cycle (Maybe (Note event))
diphone_2 = cycleFromSample S.diphone_2__Sample

diphone_3 :: forall event. Cycle (Maybe (Note event))
diphone_3 = cycleFromSample S.diphone_3__Sample

diphone_4 :: forall event. Cycle (Maybe (Note event))
diphone_4 = cycleFromSample S.diphone_4__Sample

diphone_5 :: forall event. Cycle (Maybe (Note event))
diphone_5 = cycleFromSample S.diphone_5__Sample

diphone_6 :: forall event. Cycle (Maybe (Note event))
diphone_6 = cycleFromSample S.diphone_6__Sample

diphone_7 :: forall event. Cycle (Maybe (Note event))
diphone_7 = cycleFromSample S.diphone_7__Sample

diphone_8 :: forall event. Cycle (Maybe (Note event))
diphone_8 = cycleFromSample S.diphone_8__Sample

diphone_9 :: forall event. Cycle (Maybe (Note event))
diphone_9 = cycleFromSample S.diphone_9__Sample

diphone_10 :: forall event. Cycle (Maybe (Note event))
diphone_10 = cycleFromSample S.diphone_10__Sample

diphone_11 :: forall event. Cycle (Maybe (Note event))
diphone_11 = cycleFromSample S.diphone_11__Sample

diphone_12 :: forall event. Cycle (Maybe (Note event))
diphone_12 = cycleFromSample S.diphone_12__Sample

diphone_13 :: forall event. Cycle (Maybe (Note event))
diphone_13 = cycleFromSample S.diphone_13__Sample

diphone_14 :: forall event. Cycle (Maybe (Note event))
diphone_14 = cycleFromSample S.diphone_14__Sample

diphone_15 :: forall event. Cycle (Maybe (Note event))
diphone_15 = cycleFromSample S.diphone_15__Sample

diphone_16 :: forall event. Cycle (Maybe (Note event))
diphone_16 = cycleFromSample S.diphone_16__Sample

diphone_17 :: forall event. Cycle (Maybe (Note event))
diphone_17 = cycleFromSample S.diphone_17__Sample

diphone_18 :: forall event. Cycle (Maybe (Note event))
diphone_18 = cycleFromSample S.diphone_18__Sample

diphone_19 :: forall event. Cycle (Maybe (Note event))
diphone_19 = cycleFromSample S.diphone_19__Sample

diphone_20 :: forall event. Cycle (Maybe (Note event))
diphone_20 = cycleFromSample S.diphone_20__Sample

diphone_21 :: forall event. Cycle (Maybe (Note event))
diphone_21 = cycleFromSample S.diphone_21__Sample

diphone_22 :: forall event. Cycle (Maybe (Note event))
diphone_22 = cycleFromSample S.diphone_22__Sample

diphone_23 :: forall event. Cycle (Maybe (Note event))
diphone_23 = cycleFromSample S.diphone_23__Sample

diphone_24 :: forall event. Cycle (Maybe (Note event))
diphone_24 = cycleFromSample S.diphone_24__Sample

diphone_25 :: forall event. Cycle (Maybe (Note event))
diphone_25 = cycleFromSample S.diphone_25__Sample

diphone_26 :: forall event. Cycle (Maybe (Note event))
diphone_26 = cycleFromSample S.diphone_26__Sample

diphone_27 :: forall event. Cycle (Maybe (Note event))
diphone_27 = cycleFromSample S.diphone_27__Sample

diphone_28 :: forall event. Cycle (Maybe (Note event))
diphone_28 = cycleFromSample S.diphone_28__Sample

diphone_29 :: forall event. Cycle (Maybe (Note event))
diphone_29 = cycleFromSample S.diphone_29__Sample

diphone_30 :: forall event. Cycle (Maybe (Note event))
diphone_30 = cycleFromSample S.diphone_30__Sample

diphone_31 :: forall event. Cycle (Maybe (Note event))
diphone_31 = cycleFromSample S.diphone_31__Sample

diphone_32 :: forall event. Cycle (Maybe (Note event))
diphone_32 = cycleFromSample S.diphone_32__Sample

diphone_33 :: forall event. Cycle (Maybe (Note event))
diphone_33 = cycleFromSample S.diphone_33__Sample

diphone_34 :: forall event. Cycle (Maybe (Note event))
diphone_34 = cycleFromSample S.diphone_34__Sample

diphone_35 :: forall event. Cycle (Maybe (Note event))
diphone_35 = cycleFromSample S.diphone_35__Sample

diphone_36 :: forall event. Cycle (Maybe (Note event))
diphone_36 = cycleFromSample S.diphone_36__Sample

diphone_37 :: forall event. Cycle (Maybe (Note event))
diphone_37 = cycleFromSample S.diphone_37__Sample

clak :: forall event. Cycle (Maybe (Note event))
clak = cycleFromSample S.clak_0__Sample

clak_0 :: forall event. Cycle (Maybe (Note event))
clak_0 = cycleFromSample S.clak_0__Sample

clak_1 :: forall event. Cycle (Maybe (Note event))
clak_1 = cycleFromSample S.clak_1__Sample

sitar :: forall event. Cycle (Maybe (Note event))
sitar = cycleFromSample S.sitar_0__Sample

sitar_0 :: forall event. Cycle (Maybe (Note event))
sitar_0 = cycleFromSample S.sitar_0__Sample

sitar_1 :: forall event. Cycle (Maybe (Note event))
sitar_1 = cycleFromSample S.sitar_1__Sample

sitar_2 :: forall event. Cycle (Maybe (Note event))
sitar_2 = cycleFromSample S.sitar_2__Sample

sitar_3 :: forall event. Cycle (Maybe (Note event))
sitar_3 = cycleFromSample S.sitar_3__Sample

sitar_4 :: forall event. Cycle (Maybe (Note event))
sitar_4 = cycleFromSample S.sitar_4__Sample

sitar_5 :: forall event. Cycle (Maybe (Note event))
sitar_5 = cycleFromSample S.sitar_5__Sample

sitar_6 :: forall event. Cycle (Maybe (Note event))
sitar_6 = cycleFromSample S.sitar_6__Sample

sitar_7 :: forall event. Cycle (Maybe (Note event))
sitar_7 = cycleFromSample S.sitar_7__Sample

ab :: forall event. Cycle (Maybe (Note event))
ab = cycleFromSample S.ab_0__Sample

ab_0 :: forall event. Cycle (Maybe (Note event))
ab_0 = cycleFromSample S.ab_0__Sample

ab_1 :: forall event. Cycle (Maybe (Note event))
ab_1 = cycleFromSample S.ab_1__Sample

ab_2 :: forall event. Cycle (Maybe (Note event))
ab_2 = cycleFromSample S.ab_2__Sample

ab_3 :: forall event. Cycle (Maybe (Note event))
ab_3 = cycleFromSample S.ab_3__Sample

ab_4 :: forall event. Cycle (Maybe (Note event))
ab_4 = cycleFromSample S.ab_4__Sample

ab_5 :: forall event. Cycle (Maybe (Note event))
ab_5 = cycleFromSample S.ab_5__Sample

ab_6 :: forall event. Cycle (Maybe (Note event))
ab_6 = cycleFromSample S.ab_6__Sample

ab_7 :: forall event. Cycle (Maybe (Note event))
ab_7 = cycleFromSample S.ab_7__Sample

ab_8 :: forall event. Cycle (Maybe (Note event))
ab_8 = cycleFromSample S.ab_8__Sample

ab_9 :: forall event. Cycle (Maybe (Note event))
ab_9 = cycleFromSample S.ab_9__Sample

ab_10 :: forall event. Cycle (Maybe (Note event))
ab_10 = cycleFromSample S.ab_10__Sample

ab_11 :: forall event. Cycle (Maybe (Note event))
ab_11 = cycleFromSample S.ab_11__Sample

cr :: forall event. Cycle (Maybe (Note event))
cr = cycleFromSample S.cr_0__Sample

cr_0 :: forall event. Cycle (Maybe (Note event))
cr_0 = cycleFromSample S.cr_0__Sample

cr_1 :: forall event. Cycle (Maybe (Note event))
cr_1 = cycleFromSample S.cr_1__Sample

cr_2 :: forall event. Cycle (Maybe (Note event))
cr_2 = cycleFromSample S.cr_2__Sample

cr_3 :: forall event. Cycle (Maybe (Note event))
cr_3 = cycleFromSample S.cr_3__Sample

cr_4 :: forall event. Cycle (Maybe (Note event))
cr_4 = cycleFromSample S.cr_4__Sample

cr_5 :: forall event. Cycle (Maybe (Note event))
cr_5 = cycleFromSample S.cr_5__Sample

tacscan :: forall event. Cycle (Maybe (Note event))
tacscan = cycleFromSample S.tacscan_0__Sample

tacscan_0 :: forall event. Cycle (Maybe (Note event))
tacscan_0 = cycleFromSample S.tacscan_0__Sample

tacscan_1 :: forall event. Cycle (Maybe (Note event))
tacscan_1 = cycleFromSample S.tacscan_1__Sample

tacscan_2 :: forall event. Cycle (Maybe (Note event))
tacscan_2 = cycleFromSample S.tacscan_2__Sample

tacscan_3 :: forall event. Cycle (Maybe (Note event))
tacscan_3 = cycleFromSample S.tacscan_3__Sample

tacscan_4 :: forall event. Cycle (Maybe (Note event))
tacscan_4 = cycleFromSample S.tacscan_4__Sample

tacscan_5 :: forall event. Cycle (Maybe (Note event))
tacscan_5 = cycleFromSample S.tacscan_5__Sample

tacscan_6 :: forall event. Cycle (Maybe (Note event))
tacscan_6 = cycleFromSample S.tacscan_6__Sample

tacscan_7 :: forall event. Cycle (Maybe (Note event))
tacscan_7 = cycleFromSample S.tacscan_7__Sample

tacscan_8 :: forall event. Cycle (Maybe (Note event))
tacscan_8 = cycleFromSample S.tacscan_8__Sample

tacscan_9 :: forall event. Cycle (Maybe (Note event))
tacscan_9 = cycleFromSample S.tacscan_9__Sample

tacscan_10 :: forall event. Cycle (Maybe (Note event))
tacscan_10 = cycleFromSample S.tacscan_10__Sample

tacscan_11 :: forall event. Cycle (Maybe (Note event))
tacscan_11 = cycleFromSample S.tacscan_11__Sample

tacscan_12 :: forall event. Cycle (Maybe (Note event))
tacscan_12 = cycleFromSample S.tacscan_12__Sample

tacscan_13 :: forall event. Cycle (Maybe (Note event))
tacscan_13 = cycleFromSample S.tacscan_13__Sample

tacscan_14 :: forall event. Cycle (Maybe (Note event))
tacscan_14 = cycleFromSample S.tacscan_14__Sample

tacscan_15 :: forall event. Cycle (Maybe (Note event))
tacscan_15 = cycleFromSample S.tacscan_15__Sample

tacscan_16 :: forall event. Cycle (Maybe (Note event))
tacscan_16 = cycleFromSample S.tacscan_16__Sample

tacscan_17 :: forall event. Cycle (Maybe (Note event))
tacscan_17 = cycleFromSample S.tacscan_17__Sample

tacscan_18 :: forall event. Cycle (Maybe (Note event))
tacscan_18 = cycleFromSample S.tacscan_18__Sample

tacscan_19 :: forall event. Cycle (Maybe (Note event))
tacscan_19 = cycleFromSample S.tacscan_19__Sample

tacscan_20 :: forall event. Cycle (Maybe (Note event))
tacscan_20 = cycleFromSample S.tacscan_20__Sample

tacscan_21 :: forall event. Cycle (Maybe (Note event))
tacscan_21 = cycleFromSample S.tacscan_21__Sample

v :: forall event. Cycle (Maybe (Note event))
v = cycleFromSample S.v_0__Sample

v_0 :: forall event. Cycle (Maybe (Note event))
v_0 = cycleFromSample S.v_0__Sample

v_1 :: forall event. Cycle (Maybe (Note event))
v_1 = cycleFromSample S.v_1__Sample

v_2 :: forall event. Cycle (Maybe (Note event))
v_2 = cycleFromSample S.v_2__Sample

v_3 :: forall event. Cycle (Maybe (Note event))
v_3 = cycleFromSample S.v_3__Sample

v_4 :: forall event. Cycle (Maybe (Note event))
v_4 = cycleFromSample S.v_4__Sample

v_5 :: forall event. Cycle (Maybe (Note event))
v_5 = cycleFromSample S.v_5__Sample

bd :: forall event. Cycle (Maybe (Note event))
bd = cycleFromSample S.bd_0__Sample

bd_0 :: forall event. Cycle (Maybe (Note event))
bd_0 = cycleFromSample S.bd_0__Sample

bd_1 :: forall event. Cycle (Maybe (Note event))
bd_1 = cycleFromSample S.bd_1__Sample

bd_2 :: forall event. Cycle (Maybe (Note event))
bd_2 = cycleFromSample S.bd_2__Sample

bd_3 :: forall event. Cycle (Maybe (Note event))
bd_3 = cycleFromSample S.bd_3__Sample

bd_4 :: forall event. Cycle (Maybe (Note event))
bd_4 = cycleFromSample S.bd_4__Sample

bd_5 :: forall event. Cycle (Maybe (Note event))
bd_5 = cycleFromSample S.bd_5__Sample

bd_6 :: forall event. Cycle (Maybe (Note event))
bd_6 = cycleFromSample S.bd_6__Sample

bd_7 :: forall event. Cycle (Maybe (Note event))
bd_7 = cycleFromSample S.bd_7__Sample

bd_8 :: forall event. Cycle (Maybe (Note event))
bd_8 = cycleFromSample S.bd_8__Sample

bd_9 :: forall event. Cycle (Maybe (Note event))
bd_9 = cycleFromSample S.bd_9__Sample

bd_10 :: forall event. Cycle (Maybe (Note event))
bd_10 = cycleFromSample S.bd_10__Sample

bd_11 :: forall event. Cycle (Maybe (Note event))
bd_11 = cycleFromSample S.bd_11__Sample

bd_12 :: forall event. Cycle (Maybe (Note event))
bd_12 = cycleFromSample S.bd_12__Sample

bd_13 :: forall event. Cycle (Maybe (Note event))
bd_13 = cycleFromSample S.bd_13__Sample

bd_14 :: forall event. Cycle (Maybe (Note event))
bd_14 = cycleFromSample S.bd_14__Sample

bd_15 :: forall event. Cycle (Maybe (Note event))
bd_15 = cycleFromSample S.bd_15__Sample

bd_16 :: forall event. Cycle (Maybe (Note event))
bd_16 = cycleFromSample S.bd_16__Sample

bd_17 :: forall event. Cycle (Maybe (Note event))
bd_17 = cycleFromSample S.bd_17__Sample

bd_18 :: forall event. Cycle (Maybe (Note event))
bd_18 = cycleFromSample S.bd_18__Sample

bd_19 :: forall event. Cycle (Maybe (Note event))
bd_19 = cycleFromSample S.bd_19__Sample

bd_20 :: forall event. Cycle (Maybe (Note event))
bd_20 = cycleFromSample S.bd_20__Sample

bd_21 :: forall event. Cycle (Maybe (Note event))
bd_21 = cycleFromSample S.bd_21__Sample

bd_22 :: forall event. Cycle (Maybe (Note event))
bd_22 = cycleFromSample S.bd_22__Sample

bd_23 :: forall event. Cycle (Maybe (Note event))
bd_23 = cycleFromSample S.bd_23__Sample

rm :: forall event. Cycle (Maybe (Note event))
rm = cycleFromSample S.rm_0__Sample

rm_0 :: forall event. Cycle (Maybe (Note event))
rm_0 = cycleFromSample S.rm_0__Sample

rm_1 :: forall event. Cycle (Maybe (Note event))
rm_1 = cycleFromSample S.rm_1__Sample

blue :: forall event. Cycle (Maybe (Note event))
blue = cycleFromSample S.blue_0__Sample

blue_0 :: forall event. Cycle (Maybe (Note event))
blue_0 = cycleFromSample S.blue_0__Sample

blue_1 :: forall event. Cycle (Maybe (Note event))
blue_1 = cycleFromSample S.blue_1__Sample

latibro :: forall event. Cycle (Maybe (Note event))
latibro = cycleFromSample S.latibro_0__Sample

latibro_0 :: forall event. Cycle (Maybe (Note event))
latibro_0 = cycleFromSample S.latibro_0__Sample

latibro_1 :: forall event. Cycle (Maybe (Note event))
latibro_1 = cycleFromSample S.latibro_1__Sample

latibro_2 :: forall event. Cycle (Maybe (Note event))
latibro_2 = cycleFromSample S.latibro_2__Sample

latibro_3 :: forall event. Cycle (Maybe (Note event))
latibro_3 = cycleFromSample S.latibro_3__Sample

latibro_4 :: forall event. Cycle (Maybe (Note event))
latibro_4 = cycleFromSample S.latibro_4__Sample

latibro_5 :: forall event. Cycle (Maybe (Note event))
latibro_5 = cycleFromSample S.latibro_5__Sample

latibro_6 :: forall event. Cycle (Maybe (Note event))
latibro_6 = cycleFromSample S.latibro_6__Sample

latibro_7 :: forall event. Cycle (Maybe (Note event))
latibro_7 = cycleFromSample S.latibro_7__Sample

dr_few :: forall event. Cycle (Maybe (Note event))
dr_few = cycleFromSample S.dr_few_0__Sample

dr_few_0 :: forall event. Cycle (Maybe (Note event))
dr_few_0 = cycleFromSample S.dr_few_0__Sample

dr_few_1 :: forall event. Cycle (Maybe (Note event))
dr_few_1 = cycleFromSample S.dr_few_1__Sample

dr_few_2 :: forall event. Cycle (Maybe (Note event))
dr_few_2 = cycleFromSample S.dr_few_2__Sample

dr_few_3 :: forall event. Cycle (Maybe (Note event))
dr_few_3 = cycleFromSample S.dr_few_3__Sample

dr_few_4 :: forall event. Cycle (Maybe (Note event))
dr_few_4 = cycleFromSample S.dr_few_4__Sample

dr_few_5 :: forall event. Cycle (Maybe (Note event))
dr_few_5 = cycleFromSample S.dr_few_5__Sample

dr_few_6 :: forall event. Cycle (Maybe (Note event))
dr_few_6 = cycleFromSample S.dr_few_6__Sample

dr_few_7 :: forall event. Cycle (Maybe (Note event))
dr_few_7 = cycleFromSample S.dr_few_7__Sample

rave2 :: forall event. Cycle (Maybe (Note event))
rave2 = cycleFromSample S.rave2_0__Sample

rave2_0 :: forall event. Cycle (Maybe (Note event))
rave2_0 = cycleFromSample S.rave2_0__Sample

rave2_1 :: forall event. Cycle (Maybe (Note event))
rave2_1 = cycleFromSample S.rave2_1__Sample

rave2_2 :: forall event. Cycle (Maybe (Note event))
rave2_2 = cycleFromSample S.rave2_2__Sample

rave2_3 :: forall event. Cycle (Maybe (Note event))
rave2_3 = cycleFromSample S.rave2_3__Sample

rave2_4 :: forall event. Cycle (Maybe (Note event))
rave2_4 = cycleFromSample S.rave2_4__Sample

rave2_5 :: forall event. Cycle (Maybe (Note event))
rave2_5 = cycleFromSample S.rave2_5__Sample

koy :: forall event. Cycle (Maybe (Note event))
koy = cycleFromSample S.koy_0__Sample

koy_0 :: forall event. Cycle (Maybe (Note event))
koy_0 = cycleFromSample S.koy_0__Sample

koy_1 :: forall event. Cycle (Maybe (Note event))
koy_1 = cycleFromSample S.koy_1__Sample

glitch2 :: forall event. Cycle (Maybe (Note event))
glitch2 = cycleFromSample S.glitch2_0__Sample

glitch2_0 :: forall event. Cycle (Maybe (Note event))
glitch2_0 = cycleFromSample S.glitch2_0__Sample

glitch2_1 :: forall event. Cycle (Maybe (Note event))
glitch2_1 = cycleFromSample S.glitch2_1__Sample

glitch2_2 :: forall event. Cycle (Maybe (Note event))
glitch2_2 = cycleFromSample S.glitch2_2__Sample

glitch2_3 :: forall event. Cycle (Maybe (Note event))
glitch2_3 = cycleFromSample S.glitch2_3__Sample

glitch2_4 :: forall event. Cycle (Maybe (Note event))
glitch2_4 = cycleFromSample S.glitch2_4__Sample

glitch2_5 :: forall event. Cycle (Maybe (Note event))
glitch2_5 = cycleFromSample S.glitch2_5__Sample

glitch2_6 :: forall event. Cycle (Maybe (Note event))
glitch2_6 = cycleFromSample S.glitch2_6__Sample

glitch2_7 :: forall event. Cycle (Maybe (Note event))
glitch2_7 = cycleFromSample S.glitch2_7__Sample

hmm :: forall event. Cycle (Maybe (Note event))
hmm = cycleFromSample S.hmm_0__Sample

hmm_0 :: forall event. Cycle (Maybe (Note event))
hmm_0 = cycleFromSample S.hmm_0__Sample

arp :: forall event. Cycle (Maybe (Note event))
arp = cycleFromSample S.arp_0__Sample

arp_0 :: forall event. Cycle (Maybe (Note event))
arp_0 = cycleFromSample S.arp_0__Sample

arp_1 :: forall event. Cycle (Maybe (Note event))
arp_1 = cycleFromSample S.arp_1__Sample

made2 :: forall event. Cycle (Maybe (Note event))
made2 = cycleFromSample S.made2_0__Sample

made2_0 :: forall event. Cycle (Maybe (Note event))
made2_0 = cycleFromSample S.made2_0__Sample

uxay :: forall event. Cycle (Maybe (Note event))
uxay = cycleFromSample S.uxay_0__Sample

uxay_0 :: forall event. Cycle (Maybe (Note event))
uxay_0 = cycleFromSample S.uxay_0__Sample

uxay_1 :: forall event. Cycle (Maybe (Note event))
uxay_1 = cycleFromSample S.uxay_1__Sample

uxay_2 :: forall event. Cycle (Maybe (Note event))
uxay_2 = cycleFromSample S.uxay_2__Sample

stomp :: forall event. Cycle (Maybe (Note event))
stomp = cycleFromSample S.stomp_0__Sample

stomp_0 :: forall event. Cycle (Maybe (Note event))
stomp_0 = cycleFromSample S.stomp_0__Sample

stomp_1 :: forall event. Cycle (Maybe (Note event))
stomp_1 = cycleFromSample S.stomp_1__Sample

stomp_2 :: forall event. Cycle (Maybe (Note event))
stomp_2 = cycleFromSample S.stomp_2__Sample

stomp_3 :: forall event. Cycle (Maybe (Note event))
stomp_3 = cycleFromSample S.stomp_3__Sample

stomp_4 :: forall event. Cycle (Maybe (Note event))
stomp_4 = cycleFromSample S.stomp_4__Sample

stomp_5 :: forall event. Cycle (Maybe (Note event))
stomp_5 = cycleFromSample S.stomp_5__Sample

stomp_6 :: forall event. Cycle (Maybe (Note event))
stomp_6 = cycleFromSample S.stomp_6__Sample

stomp_7 :: forall event. Cycle (Maybe (Note event))
stomp_7 = cycleFromSample S.stomp_7__Sample

stomp_8 :: forall event. Cycle (Maybe (Note event))
stomp_8 = cycleFromSample S.stomp_8__Sample

stomp_9 :: forall event. Cycle (Maybe (Note event))
stomp_9 = cycleFromSample S.stomp_9__Sample

tech :: forall event. Cycle (Maybe (Note event))
tech = cycleFromSample S.tech_0__Sample

tech_0 :: forall event. Cycle (Maybe (Note event))
tech_0 = cycleFromSample S.tech_0__Sample

tech_1 :: forall event. Cycle (Maybe (Note event))
tech_1 = cycleFromSample S.tech_1__Sample

tech_2 :: forall event. Cycle (Maybe (Note event))
tech_2 = cycleFromSample S.tech_2__Sample

tech_3 :: forall event. Cycle (Maybe (Note event))
tech_3 = cycleFromSample S.tech_3__Sample

tech_4 :: forall event. Cycle (Maybe (Note event))
tech_4 = cycleFromSample S.tech_4__Sample

tech_5 :: forall event. Cycle (Maybe (Note event))
tech_5 = cycleFromSample S.tech_5__Sample

tech_6 :: forall event. Cycle (Maybe (Note event))
tech_6 = cycleFromSample S.tech_6__Sample

tech_7 :: forall event. Cycle (Maybe (Note event))
tech_7 = cycleFromSample S.tech_7__Sample

tech_8 :: forall event. Cycle (Maybe (Note event))
tech_8 = cycleFromSample S.tech_8__Sample

tech_9 :: forall event. Cycle (Maybe (Note event))
tech_9 = cycleFromSample S.tech_9__Sample

tech_10 :: forall event. Cycle (Maybe (Note event))
tech_10 = cycleFromSample S.tech_10__Sample

tech_11 :: forall event. Cycle (Maybe (Note event))
tech_11 = cycleFromSample S.tech_11__Sample

tech_12 :: forall event. Cycle (Maybe (Note event))
tech_12 = cycleFromSample S.tech_12__Sample

sn :: forall event. Cycle (Maybe (Note event))
sn = cycleFromSample S.sn_0__Sample

sn_0 :: forall event. Cycle (Maybe (Note event))
sn_0 = cycleFromSample S.sn_0__Sample

sn_1 :: forall event. Cycle (Maybe (Note event))
sn_1 = cycleFromSample S.sn_1__Sample

sn_2 :: forall event. Cycle (Maybe (Note event))
sn_2 = cycleFromSample S.sn_2__Sample

sn_3 :: forall event. Cycle (Maybe (Note event))
sn_3 = cycleFromSample S.sn_3__Sample

sn_4 :: forall event. Cycle (Maybe (Note event))
sn_4 = cycleFromSample S.sn_4__Sample

sn_5 :: forall event. Cycle (Maybe (Note event))
sn_5 = cycleFromSample S.sn_5__Sample

sn_6 :: forall event. Cycle (Maybe (Note event))
sn_6 = cycleFromSample S.sn_6__Sample

sn_7 :: forall event. Cycle (Maybe (Note event))
sn_7 = cycleFromSample S.sn_7__Sample

sn_8 :: forall event. Cycle (Maybe (Note event))
sn_8 = cycleFromSample S.sn_8__Sample

sn_9 :: forall event. Cycle (Maybe (Note event))
sn_9 = cycleFromSample S.sn_9__Sample

sn_10 :: forall event. Cycle (Maybe (Note event))
sn_10 = cycleFromSample S.sn_10__Sample

sn_11 :: forall event. Cycle (Maybe (Note event))
sn_11 = cycleFromSample S.sn_11__Sample

sn_12 :: forall event. Cycle (Maybe (Note event))
sn_12 = cycleFromSample S.sn_12__Sample

sn_13 :: forall event. Cycle (Maybe (Note event))
sn_13 = cycleFromSample S.sn_13__Sample

sn_14 :: forall event. Cycle (Maybe (Note event))
sn_14 = cycleFromSample S.sn_14__Sample

sn_15 :: forall event. Cycle (Maybe (Note event))
sn_15 = cycleFromSample S.sn_15__Sample

sn_16 :: forall event. Cycle (Maybe (Note event))
sn_16 = cycleFromSample S.sn_16__Sample

sn_17 :: forall event. Cycle (Maybe (Note event))
sn_17 = cycleFromSample S.sn_17__Sample

sn_18 :: forall event. Cycle (Maybe (Note event))
sn_18 = cycleFromSample S.sn_18__Sample

sn_19 :: forall event. Cycle (Maybe (Note event))
sn_19 = cycleFromSample S.sn_19__Sample

sn_20 :: forall event. Cycle (Maybe (Note event))
sn_20 = cycleFromSample S.sn_20__Sample

sn_21 :: forall event. Cycle (Maybe (Note event))
sn_21 = cycleFromSample S.sn_21__Sample

sn_22 :: forall event. Cycle (Maybe (Note event))
sn_22 = cycleFromSample S.sn_22__Sample

sn_23 :: forall event. Cycle (Maybe (Note event))
sn_23 = cycleFromSample S.sn_23__Sample

sn_24 :: forall event. Cycle (Maybe (Note event))
sn_24 = cycleFromSample S.sn_24__Sample

sn_25 :: forall event. Cycle (Maybe (Note event))
sn_25 = cycleFromSample S.sn_25__Sample

sn_26 :: forall event. Cycle (Maybe (Note event))
sn_26 = cycleFromSample S.sn_26__Sample

sn_27 :: forall event. Cycle (Maybe (Note event))
sn_27 = cycleFromSample S.sn_27__Sample

sn_28 :: forall event. Cycle (Maybe (Note event))
sn_28 = cycleFromSample S.sn_28__Sample

sn_29 :: forall event. Cycle (Maybe (Note event))
sn_29 = cycleFromSample S.sn_29__Sample

sn_30 :: forall event. Cycle (Maybe (Note event))
sn_30 = cycleFromSample S.sn_30__Sample

sn_31 :: forall event. Cycle (Maybe (Note event))
sn_31 = cycleFromSample S.sn_31__Sample

sn_32 :: forall event. Cycle (Maybe (Note event))
sn_32 = cycleFromSample S.sn_32__Sample

sn_33 :: forall event. Cycle (Maybe (Note event))
sn_33 = cycleFromSample S.sn_33__Sample

sn_34 :: forall event. Cycle (Maybe (Note event))
sn_34 = cycleFromSample S.sn_34__Sample

sn_35 :: forall event. Cycle (Maybe (Note event))
sn_35 = cycleFromSample S.sn_35__Sample

sn_36 :: forall event. Cycle (Maybe (Note event))
sn_36 = cycleFromSample S.sn_36__Sample

sn_37 :: forall event. Cycle (Maybe (Note event))
sn_37 = cycleFromSample S.sn_37__Sample

sn_38 :: forall event. Cycle (Maybe (Note event))
sn_38 = cycleFromSample S.sn_38__Sample

sn_39 :: forall event. Cycle (Maybe (Note event))
sn_39 = cycleFromSample S.sn_39__Sample

sn_40 :: forall event. Cycle (Maybe (Note event))
sn_40 = cycleFromSample S.sn_40__Sample

sn_41 :: forall event. Cycle (Maybe (Note event))
sn_41 = cycleFromSample S.sn_41__Sample

sn_42 :: forall event. Cycle (Maybe (Note event))
sn_42 = cycleFromSample S.sn_42__Sample

sn_43 :: forall event. Cycle (Maybe (Note event))
sn_43 = cycleFromSample S.sn_43__Sample

sn_44 :: forall event. Cycle (Maybe (Note event))
sn_44 = cycleFromSample S.sn_44__Sample

sn_45 :: forall event. Cycle (Maybe (Note event))
sn_45 = cycleFromSample S.sn_45__Sample

sn_46 :: forall event. Cycle (Maybe (Note event))
sn_46 = cycleFromSample S.sn_46__Sample

sn_47 :: forall event. Cycle (Maybe (Note event))
sn_47 = cycleFromSample S.sn_47__Sample

sn_48 :: forall event. Cycle (Maybe (Note event))
sn_48 = cycleFromSample S.sn_48__Sample

sn_49 :: forall event. Cycle (Maybe (Note event))
sn_49 = cycleFromSample S.sn_49__Sample

sn_50 :: forall event. Cycle (Maybe (Note event))
sn_50 = cycleFromSample S.sn_50__Sample

sn_51 :: forall event. Cycle (Maybe (Note event))
sn_51 = cycleFromSample S.sn_51__Sample

less :: forall event. Cycle (Maybe (Note event))
less = cycleFromSample S.less_0__Sample

less_0 :: forall event. Cycle (Maybe (Note event))
less_0 = cycleFromSample S.less_0__Sample

less_1 :: forall event. Cycle (Maybe (Note event))
less_1 = cycleFromSample S.less_1__Sample

less_2 :: forall event. Cycle (Maybe (Note event))
less_2 = cycleFromSample S.less_2__Sample

less_3 :: forall event. Cycle (Maybe (Note event))
less_3 = cycleFromSample S.less_3__Sample

off :: forall event. Cycle (Maybe (Note event))
off = cycleFromSample S.off_0__Sample

off_0 :: forall event. Cycle (Maybe (Note event))
off_0 = cycleFromSample S.off_0__Sample

x_808sd :: forall event. Cycle (Maybe (Note event))
x_808sd = cycleFromSample S.x_808sd_0__Sample

x_808sd_0 :: forall event. Cycle (Maybe (Note event))
x_808sd_0 = cycleFromSample S.x_808sd_0__Sample

x_808sd_1 :: forall event. Cycle (Maybe (Note event))
x_808sd_1 = cycleFromSample S.x_808sd_1__Sample

x_808sd_2 :: forall event. Cycle (Maybe (Note event))
x_808sd_2 = cycleFromSample S.x_808sd_2__Sample

x_808sd_3 :: forall event. Cycle (Maybe (Note event))
x_808sd_3 = cycleFromSample S.x_808sd_3__Sample

x_808sd_4 :: forall event. Cycle (Maybe (Note event))
x_808sd_4 = cycleFromSample S.x_808sd_4__Sample

x_808sd_5 :: forall event. Cycle (Maybe (Note event))
x_808sd_5 = cycleFromSample S.x_808sd_5__Sample

x_808sd_6 :: forall event. Cycle (Maybe (Note event))
x_808sd_6 = cycleFromSample S.x_808sd_6__Sample

x_808sd_7 :: forall event. Cycle (Maybe (Note event))
x_808sd_7 = cycleFromSample S.x_808sd_7__Sample

x_808sd_8 :: forall event. Cycle (Maybe (Note event))
x_808sd_8 = cycleFromSample S.x_808sd_8__Sample

x_808sd_9 :: forall event. Cycle (Maybe (Note event))
x_808sd_9 = cycleFromSample S.x_808sd_9__Sample

x_808sd_10 :: forall event. Cycle (Maybe (Note event))
x_808sd_10 = cycleFromSample S.x_808sd_10__Sample

x_808sd_11 :: forall event. Cycle (Maybe (Note event))
x_808sd_11 = cycleFromSample S.x_808sd_11__Sample

x_808sd_12 :: forall event. Cycle (Maybe (Note event))
x_808sd_12 = cycleFromSample S.x_808sd_12__Sample

x_808sd_13 :: forall event. Cycle (Maybe (Note event))
x_808sd_13 = cycleFromSample S.x_808sd_13__Sample

x_808sd_14 :: forall event. Cycle (Maybe (Note event))
x_808sd_14 = cycleFromSample S.x_808sd_14__Sample

x_808sd_15 :: forall event. Cycle (Maybe (Note event))
x_808sd_15 = cycleFromSample S.x_808sd_15__Sample

x_808sd_16 :: forall event. Cycle (Maybe (Note event))
x_808sd_16 = cycleFromSample S.x_808sd_16__Sample

x_808sd_17 :: forall event. Cycle (Maybe (Note event))
x_808sd_17 = cycleFromSample S.x_808sd_17__Sample

x_808sd_18 :: forall event. Cycle (Maybe (Note event))
x_808sd_18 = cycleFromSample S.x_808sd_18__Sample

x_808sd_19 :: forall event. Cycle (Maybe (Note event))
x_808sd_19 = cycleFromSample S.x_808sd_19__Sample

x_808sd_20 :: forall event. Cycle (Maybe (Note event))
x_808sd_20 = cycleFromSample S.x_808sd_20__Sample

x_808sd_21 :: forall event. Cycle (Maybe (Note event))
x_808sd_21 = cycleFromSample S.x_808sd_21__Sample

x_808sd_22 :: forall event. Cycle (Maybe (Note event))
x_808sd_22 = cycleFromSample S.x_808sd_22__Sample

x_808sd_23 :: forall event. Cycle (Maybe (Note event))
x_808sd_23 = cycleFromSample S.x_808sd_23__Sample

x_808sd_24 :: forall event. Cycle (Maybe (Note event))
x_808sd_24 = cycleFromSample S.x_808sd_24__Sample

trump :: forall event. Cycle (Maybe (Note event))
trump = cycleFromSample S.trump_0__Sample

trump_0 :: forall event. Cycle (Maybe (Note event))
trump_0 = cycleFromSample S.trump_0__Sample

trump_1 :: forall event. Cycle (Maybe (Note event))
trump_1 = cycleFromSample S.trump_1__Sample

trump_2 :: forall event. Cycle (Maybe (Note event))
trump_2 = cycleFromSample S.trump_2__Sample

trump_3 :: forall event. Cycle (Maybe (Note event))
trump_3 = cycleFromSample S.trump_3__Sample

trump_4 :: forall event. Cycle (Maybe (Note event))
trump_4 = cycleFromSample S.trump_4__Sample

trump_5 :: forall event. Cycle (Maybe (Note event))
trump_5 = cycleFromSample S.trump_5__Sample

trump_6 :: forall event. Cycle (Maybe (Note event))
trump_6 = cycleFromSample S.trump_6__Sample

trump_7 :: forall event. Cycle (Maybe (Note event))
trump_7 = cycleFromSample S.trump_7__Sample

trump_8 :: forall event. Cycle (Maybe (Note event))
trump_8 = cycleFromSample S.trump_8__Sample

trump_9 :: forall event. Cycle (Maybe (Note event))
trump_9 = cycleFromSample S.trump_9__Sample

trump_10 :: forall event. Cycle (Maybe (Note event))
trump_10 = cycleFromSample S.trump_10__Sample

bev :: forall event. Cycle (Maybe (Note event))
bev = cycleFromSample S.bev_0__Sample

bev_0 :: forall event. Cycle (Maybe (Note event))
bev_0 = cycleFromSample S.bev_0__Sample

bev_1 :: forall event. Cycle (Maybe (Note event))
bev_1 = cycleFromSample S.bev_1__Sample

pad :: forall event. Cycle (Maybe (Note event))
pad = cycleFromSample S.pad_0__Sample

pad_0 :: forall event. Cycle (Maybe (Note event))
pad_0 = cycleFromSample S.pad_0__Sample

pad_1 :: forall event. Cycle (Maybe (Note event))
pad_1 = cycleFromSample S.pad_1__Sample

pad_2 :: forall event. Cycle (Maybe (Note event))
pad_2 = cycleFromSample S.pad_2__Sample

led :: forall event. Cycle (Maybe (Note event))
led = cycleFromSample S.led_0__Sample

led_0 :: forall event. Cycle (Maybe (Note event))
led_0 = cycleFromSample S.led_0__Sample

perc :: forall event. Cycle (Maybe (Note event))
perc = cycleFromSample S.perc_0__Sample

perc_0 :: forall event. Cycle (Maybe (Note event))
perc_0 = cycleFromSample S.perc_0__Sample

perc_1 :: forall event. Cycle (Maybe (Note event))
perc_1 = cycleFromSample S.perc_1__Sample

perc_2 :: forall event. Cycle (Maybe (Note event))
perc_2 = cycleFromSample S.perc_2__Sample

perc_3 :: forall event. Cycle (Maybe (Note event))
perc_3 = cycleFromSample S.perc_3__Sample

perc_4 :: forall event. Cycle (Maybe (Note event))
perc_4 = cycleFromSample S.perc_4__Sample

perc_5 :: forall event. Cycle (Maybe (Note event))
perc_5 = cycleFromSample S.perc_5__Sample

pluck :: forall event. Cycle (Maybe (Note event))
pluck = cycleFromSample S.pluck_0__Sample

pluck_0 :: forall event. Cycle (Maybe (Note event))
pluck_0 = cycleFromSample S.pluck_0__Sample

pluck_1 :: forall event. Cycle (Maybe (Note event))
pluck_1 = cycleFromSample S.pluck_1__Sample

pluck_2 :: forall event. Cycle (Maybe (Note event))
pluck_2 = cycleFromSample S.pluck_2__Sample

pluck_3 :: forall event. Cycle (Maybe (Note event))
pluck_3 = cycleFromSample S.pluck_3__Sample

pluck_4 :: forall event. Cycle (Maybe (Note event))
pluck_4 = cycleFromSample S.pluck_4__Sample

pluck_5 :: forall event. Cycle (Maybe (Note event))
pluck_5 = cycleFromSample S.pluck_5__Sample

pluck_6 :: forall event. Cycle (Maybe (Note event))
pluck_6 = cycleFromSample S.pluck_6__Sample

pluck_7 :: forall event. Cycle (Maybe (Note event))
pluck_7 = cycleFromSample S.pluck_7__Sample

pluck_8 :: forall event. Cycle (Maybe (Note event))
pluck_8 = cycleFromSample S.pluck_8__Sample

pluck_9 :: forall event. Cycle (Maybe (Note event))
pluck_9 = cycleFromSample S.pluck_9__Sample

pluck_10 :: forall event. Cycle (Maybe (Note event))
pluck_10 = cycleFromSample S.pluck_10__Sample

pluck_11 :: forall event. Cycle (Maybe (Note event))
pluck_11 = cycleFromSample S.pluck_11__Sample

pluck_12 :: forall event. Cycle (Maybe (Note event))
pluck_12 = cycleFromSample S.pluck_12__Sample

pluck_13 :: forall event. Cycle (Maybe (Note event))
pluck_13 = cycleFromSample S.pluck_13__Sample

pluck_14 :: forall event. Cycle (Maybe (Note event))
pluck_14 = cycleFromSample S.pluck_14__Sample

pluck_15 :: forall event. Cycle (Maybe (Note event))
pluck_15 = cycleFromSample S.pluck_15__Sample

pluck_16 :: forall event. Cycle (Maybe (Note event))
pluck_16 = cycleFromSample S.pluck_16__Sample

bleep :: forall event. Cycle (Maybe (Note event))
bleep = cycleFromSample S.bleep_0__Sample

bleep_0 :: forall event. Cycle (Maybe (Note event))
bleep_0 = cycleFromSample S.bleep_0__Sample

bleep_1 :: forall event. Cycle (Maybe (Note event))
bleep_1 = cycleFromSample S.bleep_1__Sample

bleep_2 :: forall event. Cycle (Maybe (Note event))
bleep_2 = cycleFromSample S.bleep_2__Sample

bleep_3 :: forall event. Cycle (Maybe (Note event))
bleep_3 = cycleFromSample S.bleep_3__Sample

bleep_4 :: forall event. Cycle (Maybe (Note event))
bleep_4 = cycleFromSample S.bleep_4__Sample

bleep_5 :: forall event. Cycle (Maybe (Note event))
bleep_5 = cycleFromSample S.bleep_5__Sample

bleep_6 :: forall event. Cycle (Maybe (Note event))
bleep_6 = cycleFromSample S.bleep_6__Sample

bleep_7 :: forall event. Cycle (Maybe (Note event))
bleep_7 = cycleFromSample S.bleep_7__Sample

bleep_8 :: forall event. Cycle (Maybe (Note event))
bleep_8 = cycleFromSample S.bleep_8__Sample

bleep_9 :: forall event. Cycle (Maybe (Note event))
bleep_9 = cycleFromSample S.bleep_9__Sample

bleep_10 :: forall event. Cycle (Maybe (Note event))
bleep_10 = cycleFromSample S.bleep_10__Sample

bleep_11 :: forall event. Cycle (Maybe (Note event))
bleep_11 = cycleFromSample S.bleep_11__Sample

bleep_12 :: forall event. Cycle (Maybe (Note event))
bleep_12 = cycleFromSample S.bleep_12__Sample

ht :: forall event. Cycle (Maybe (Note event))
ht = cycleFromSample S.ht_0__Sample

ht_0 :: forall event. Cycle (Maybe (Note event))
ht_0 = cycleFromSample S.ht_0__Sample

ht_1 :: forall event. Cycle (Maybe (Note event))
ht_1 = cycleFromSample S.ht_1__Sample

ht_2 :: forall event. Cycle (Maybe (Note event))
ht_2 = cycleFromSample S.ht_2__Sample

ht_3 :: forall event. Cycle (Maybe (Note event))
ht_3 = cycleFromSample S.ht_3__Sample

ht_4 :: forall event. Cycle (Maybe (Note event))
ht_4 = cycleFromSample S.ht_4__Sample

ht_5 :: forall event. Cycle (Maybe (Note event))
ht_5 = cycleFromSample S.ht_5__Sample

ht_6 :: forall event. Cycle (Maybe (Note event))
ht_6 = cycleFromSample S.ht_6__Sample

ht_7 :: forall event. Cycle (Maybe (Note event))
ht_7 = cycleFromSample S.ht_7__Sample

ht_8 :: forall event. Cycle (Maybe (Note event))
ht_8 = cycleFromSample S.ht_8__Sample

ht_9 :: forall event. Cycle (Maybe (Note event))
ht_9 = cycleFromSample S.ht_9__Sample

ht_10 :: forall event. Cycle (Maybe (Note event))
ht_10 = cycleFromSample S.ht_10__Sample

ht_11 :: forall event. Cycle (Maybe (Note event))
ht_11 = cycleFromSample S.ht_11__Sample

ht_12 :: forall event. Cycle (Maybe (Note event))
ht_12 = cycleFromSample S.ht_12__Sample

ht_13 :: forall event. Cycle (Maybe (Note event))
ht_13 = cycleFromSample S.ht_13__Sample

ht_14 :: forall event. Cycle (Maybe (Note event))
ht_14 = cycleFromSample S.ht_14__Sample

ht_15 :: forall event. Cycle (Maybe (Note event))
ht_15 = cycleFromSample S.ht_15__Sample

ades4 :: forall event. Cycle (Maybe (Note event))
ades4 = cycleFromSample S.ades4_0__Sample

ades4_0 :: forall event. Cycle (Maybe (Note event))
ades4_0 = cycleFromSample S.ades4_0__Sample

ades4_1 :: forall event. Cycle (Maybe (Note event))
ades4_1 = cycleFromSample S.ades4_1__Sample

ades4_2 :: forall event. Cycle (Maybe (Note event))
ades4_2 = cycleFromSample S.ades4_2__Sample

ades4_3 :: forall event. Cycle (Maybe (Note event))
ades4_3 = cycleFromSample S.ades4_3__Sample

ades4_4 :: forall event. Cycle (Maybe (Note event))
ades4_4 = cycleFromSample S.ades4_4__Sample

ades4_5 :: forall event. Cycle (Maybe (Note event))
ades4_5 = cycleFromSample S.ades4_5__Sample

proc :: forall event. Cycle (Maybe (Note event))
proc = cycleFromSample S.proc_0__Sample

proc_0 :: forall event. Cycle (Maybe (Note event))
proc_0 = cycleFromSample S.proc_0__Sample

proc_1 :: forall event. Cycle (Maybe (Note event))
proc_1 = cycleFromSample S.proc_1__Sample

gretsch :: forall event. Cycle (Maybe (Note event))
gretsch = cycleFromSample S.gretsch_0__Sample

gretsch_0 :: forall event. Cycle (Maybe (Note event))
gretsch_0 = cycleFromSample S.gretsch_0__Sample

gretsch_1 :: forall event. Cycle (Maybe (Note event))
gretsch_1 = cycleFromSample S.gretsch_1__Sample

gretsch_2 :: forall event. Cycle (Maybe (Note event))
gretsch_2 = cycleFromSample S.gretsch_2__Sample

gretsch_3 :: forall event. Cycle (Maybe (Note event))
gretsch_3 = cycleFromSample S.gretsch_3__Sample

gretsch_4 :: forall event. Cycle (Maybe (Note event))
gretsch_4 = cycleFromSample S.gretsch_4__Sample

gretsch_5 :: forall event. Cycle (Maybe (Note event))
gretsch_5 = cycleFromSample S.gretsch_5__Sample

gretsch_6 :: forall event. Cycle (Maybe (Note event))
gretsch_6 = cycleFromSample S.gretsch_6__Sample

gretsch_7 :: forall event. Cycle (Maybe (Note event))
gretsch_7 = cycleFromSample S.gretsch_7__Sample

gretsch_8 :: forall event. Cycle (Maybe (Note event))
gretsch_8 = cycleFromSample S.gretsch_8__Sample

gretsch_9 :: forall event. Cycle (Maybe (Note event))
gretsch_9 = cycleFromSample S.gretsch_9__Sample

gretsch_10 :: forall event. Cycle (Maybe (Note event))
gretsch_10 = cycleFromSample S.gretsch_10__Sample

gretsch_11 :: forall event. Cycle (Maybe (Note event))
gretsch_11 = cycleFromSample S.gretsch_11__Sample

gretsch_12 :: forall event. Cycle (Maybe (Note event))
gretsch_12 = cycleFromSample S.gretsch_12__Sample

gretsch_13 :: forall event. Cycle (Maybe (Note event))
gretsch_13 = cycleFromSample S.gretsch_13__Sample

gretsch_14 :: forall event. Cycle (Maybe (Note event))
gretsch_14 = cycleFromSample S.gretsch_14__Sample

gretsch_15 :: forall event. Cycle (Maybe (Note event))
gretsch_15 = cycleFromSample S.gretsch_15__Sample

gretsch_16 :: forall event. Cycle (Maybe (Note event))
gretsch_16 = cycleFromSample S.gretsch_16__Sample

gretsch_17 :: forall event. Cycle (Maybe (Note event))
gretsch_17 = cycleFromSample S.gretsch_17__Sample

gretsch_18 :: forall event. Cycle (Maybe (Note event))
gretsch_18 = cycleFromSample S.gretsch_18__Sample

gretsch_19 :: forall event. Cycle (Maybe (Note event))
gretsch_19 = cycleFromSample S.gretsch_19__Sample

gretsch_20 :: forall event. Cycle (Maybe (Note event))
gretsch_20 = cycleFromSample S.gretsch_20__Sample

gretsch_21 :: forall event. Cycle (Maybe (Note event))
gretsch_21 = cycleFromSample S.gretsch_21__Sample

gretsch_22 :: forall event. Cycle (Maybe (Note event))
gretsch_22 = cycleFromSample S.gretsch_22__Sample

gretsch_23 :: forall event. Cycle (Maybe (Note event))
gretsch_23 = cycleFromSample S.gretsch_23__Sample

outdoor :: forall event. Cycle (Maybe (Note event))
outdoor = cycleFromSample S.outdoor_0__Sample

outdoor_0 :: forall event. Cycle (Maybe (Note event))
outdoor_0 = cycleFromSample S.outdoor_0__Sample

outdoor_1 :: forall event. Cycle (Maybe (Note event))
outdoor_1 = cycleFromSample S.outdoor_1__Sample

outdoor_2 :: forall event. Cycle (Maybe (Note event))
outdoor_2 = cycleFromSample S.outdoor_2__Sample

outdoor_3 :: forall event. Cycle (Maybe (Note event))
outdoor_3 = cycleFromSample S.outdoor_3__Sample

outdoor_4 :: forall event. Cycle (Maybe (Note event))
outdoor_4 = cycleFromSample S.outdoor_4__Sample

outdoor_5 :: forall event. Cycle (Maybe (Note event))
outdoor_5 = cycleFromSample S.outdoor_5__Sample

techno :: forall event. Cycle (Maybe (Note event))
techno = cycleFromSample S.techno_0__Sample

techno_0 :: forall event. Cycle (Maybe (Note event))
techno_0 = cycleFromSample S.techno_0__Sample

techno_1 :: forall event. Cycle (Maybe (Note event))
techno_1 = cycleFromSample S.techno_1__Sample

techno_2 :: forall event. Cycle (Maybe (Note event))
techno_2 = cycleFromSample S.techno_2__Sample

techno_3 :: forall event. Cycle (Maybe (Note event))
techno_3 = cycleFromSample S.techno_3__Sample

techno_4 :: forall event. Cycle (Maybe (Note event))
techno_4 = cycleFromSample S.techno_4__Sample

techno_5 :: forall event. Cycle (Maybe (Note event))
techno_5 = cycleFromSample S.techno_5__Sample

techno_6 :: forall event. Cycle (Maybe (Note event))
techno_6 = cycleFromSample S.techno_6__Sample

ulgab :: forall event. Cycle (Maybe (Note event))
ulgab = cycleFromSample S.ulgab_0__Sample

ulgab_0 :: forall event. Cycle (Maybe (Note event))
ulgab_0 = cycleFromSample S.ulgab_0__Sample

ulgab_1 :: forall event. Cycle (Maybe (Note event))
ulgab_1 = cycleFromSample S.ulgab_1__Sample

ulgab_2 :: forall event. Cycle (Maybe (Note event))
ulgab_2 = cycleFromSample S.ulgab_2__Sample

ulgab_3 :: forall event. Cycle (Maybe (Note event))
ulgab_3 = cycleFromSample S.ulgab_3__Sample

ulgab_4 :: forall event. Cycle (Maybe (Note event))
ulgab_4 = cycleFromSample S.ulgab_4__Sample

breaks125 :: forall event. Cycle (Maybe (Note event))
breaks125 = cycleFromSample S.breaks125_0__Sample

breaks125_0 :: forall event. Cycle (Maybe (Note event))
breaks125_0 = cycleFromSample S.breaks125_0__Sample

breaks125_1 :: forall event. Cycle (Maybe (Note event))
breaks125_1 = cycleFromSample S.breaks125_1__Sample

bin :: forall event. Cycle (Maybe (Note event))
bin = cycleFromSample S.bin_0__Sample

bin_0 :: forall event. Cycle (Maybe (Note event))
bin_0 = cycleFromSample S.bin_0__Sample

bin_1 :: forall event. Cycle (Maybe (Note event))
bin_1 = cycleFromSample S.bin_1__Sample

x_808mc :: forall event. Cycle (Maybe (Note event))
x_808mc = cycleFromSample S.x_808mc_0__Sample

x_808mc_0 :: forall event. Cycle (Maybe (Note event))
x_808mc_0 = cycleFromSample S.x_808mc_0__Sample

x_808mc_1 :: forall event. Cycle (Maybe (Note event))
x_808mc_1 = cycleFromSample S.x_808mc_1__Sample

x_808mc_2 :: forall event. Cycle (Maybe (Note event))
x_808mc_2 = cycleFromSample S.x_808mc_2__Sample

x_808mc_3 :: forall event. Cycle (Maybe (Note event))
x_808mc_3 = cycleFromSample S.x_808mc_3__Sample

x_808mc_4 :: forall event. Cycle (Maybe (Note event))
x_808mc_4 = cycleFromSample S.x_808mc_4__Sample

lt :: forall event. Cycle (Maybe (Note event))
lt = cycleFromSample S.lt_0__Sample

lt_0 :: forall event. Cycle (Maybe (Note event))
lt_0 = cycleFromSample S.lt_0__Sample

lt_1 :: forall event. Cycle (Maybe (Note event))
lt_1 = cycleFromSample S.lt_1__Sample

lt_2 :: forall event. Cycle (Maybe (Note event))
lt_2 = cycleFromSample S.lt_2__Sample

lt_3 :: forall event. Cycle (Maybe (Note event))
lt_3 = cycleFromSample S.lt_3__Sample

lt_4 :: forall event. Cycle (Maybe (Note event))
lt_4 = cycleFromSample S.lt_4__Sample

lt_5 :: forall event. Cycle (Maybe (Note event))
lt_5 = cycleFromSample S.lt_5__Sample

lt_6 :: forall event. Cycle (Maybe (Note event))
lt_6 = cycleFromSample S.lt_6__Sample

lt_7 :: forall event. Cycle (Maybe (Note event))
lt_7 = cycleFromSample S.lt_7__Sample

lt_8 :: forall event. Cycle (Maybe (Note event))
lt_8 = cycleFromSample S.lt_8__Sample

lt_9 :: forall event. Cycle (Maybe (Note event))
lt_9 = cycleFromSample S.lt_9__Sample

lt_10 :: forall event. Cycle (Maybe (Note event))
lt_10 = cycleFromSample S.lt_10__Sample

lt_11 :: forall event. Cycle (Maybe (Note event))
lt_11 = cycleFromSample S.lt_11__Sample

lt_12 :: forall event. Cycle (Maybe (Note event))
lt_12 = cycleFromSample S.lt_12__Sample

lt_13 :: forall event. Cycle (Maybe (Note event))
lt_13 = cycleFromSample S.lt_13__Sample

lt_14 :: forall event. Cycle (Maybe (Note event))
lt_14 = cycleFromSample S.lt_14__Sample

lt_15 :: forall event. Cycle (Maybe (Note event))
lt_15 = cycleFromSample S.lt_15__Sample

amencutup :: forall event. Cycle (Maybe (Note event))
amencutup = cycleFromSample S.amencutup_0__Sample

amencutup_0 :: forall event. Cycle (Maybe (Note event))
amencutup_0 = cycleFromSample S.amencutup_0__Sample

amencutup_1 :: forall event. Cycle (Maybe (Note event))
amencutup_1 = cycleFromSample S.amencutup_1__Sample

amencutup_2 :: forall event. Cycle (Maybe (Note event))
amencutup_2 = cycleFromSample S.amencutup_2__Sample

amencutup_3 :: forall event. Cycle (Maybe (Note event))
amencutup_3 = cycleFromSample S.amencutup_3__Sample

amencutup_4 :: forall event. Cycle (Maybe (Note event))
amencutup_4 = cycleFromSample S.amencutup_4__Sample

amencutup_5 :: forall event. Cycle (Maybe (Note event))
amencutup_5 = cycleFromSample S.amencutup_5__Sample

amencutup_6 :: forall event. Cycle (Maybe (Note event))
amencutup_6 = cycleFromSample S.amencutup_6__Sample

amencutup_7 :: forall event. Cycle (Maybe (Note event))
amencutup_7 = cycleFromSample S.amencutup_7__Sample

amencutup_8 :: forall event. Cycle (Maybe (Note event))
amencutup_8 = cycleFromSample S.amencutup_8__Sample

amencutup_9 :: forall event. Cycle (Maybe (Note event))
amencutup_9 = cycleFromSample S.amencutup_9__Sample

amencutup_10 :: forall event. Cycle (Maybe (Note event))
amencutup_10 = cycleFromSample S.amencutup_10__Sample

amencutup_11 :: forall event. Cycle (Maybe (Note event))
amencutup_11 = cycleFromSample S.amencutup_11__Sample

amencutup_12 :: forall event. Cycle (Maybe (Note event))
amencutup_12 = cycleFromSample S.amencutup_12__Sample

amencutup_13 :: forall event. Cycle (Maybe (Note event))
amencutup_13 = cycleFromSample S.amencutup_13__Sample

amencutup_14 :: forall event. Cycle (Maybe (Note event))
amencutup_14 = cycleFromSample S.amencutup_14__Sample

amencutup_15 :: forall event. Cycle (Maybe (Note event))
amencutup_15 = cycleFromSample S.amencutup_15__Sample

amencutup_16 :: forall event. Cycle (Maybe (Note event))
amencutup_16 = cycleFromSample S.amencutup_16__Sample

amencutup_17 :: forall event. Cycle (Maybe (Note event))
amencutup_17 = cycleFromSample S.amencutup_17__Sample

amencutup_18 :: forall event. Cycle (Maybe (Note event))
amencutup_18 = cycleFromSample S.amencutup_18__Sample

amencutup_19 :: forall event. Cycle (Maybe (Note event))
amencutup_19 = cycleFromSample S.amencutup_19__Sample

amencutup_20 :: forall event. Cycle (Maybe (Note event))
amencutup_20 = cycleFromSample S.amencutup_20__Sample

amencutup_21 :: forall event. Cycle (Maybe (Note event))
amencutup_21 = cycleFromSample S.amencutup_21__Sample

amencutup_22 :: forall event. Cycle (Maybe (Note event))
amencutup_22 = cycleFromSample S.amencutup_22__Sample

amencutup_23 :: forall event. Cycle (Maybe (Note event))
amencutup_23 = cycleFromSample S.amencutup_23__Sample

amencutup_24 :: forall event. Cycle (Maybe (Note event))
amencutup_24 = cycleFromSample S.amencutup_24__Sample

amencutup_25 :: forall event. Cycle (Maybe (Note event))
amencutup_25 = cycleFromSample S.amencutup_25__Sample

amencutup_26 :: forall event. Cycle (Maybe (Note event))
amencutup_26 = cycleFromSample S.amencutup_26__Sample

amencutup_27 :: forall event. Cycle (Maybe (Note event))
amencutup_27 = cycleFromSample S.amencutup_27__Sample

amencutup_28 :: forall event. Cycle (Maybe (Note event))
amencutup_28 = cycleFromSample S.amencutup_28__Sample

amencutup_29 :: forall event. Cycle (Maybe (Note event))
amencutup_29 = cycleFromSample S.amencutup_29__Sample

amencutup_30 :: forall event. Cycle (Maybe (Note event))
amencutup_30 = cycleFromSample S.amencutup_30__Sample

amencutup_31 :: forall event. Cycle (Maybe (Note event))
amencutup_31 = cycleFromSample S.amencutup_31__Sample

drum :: forall event. Cycle (Maybe (Note event))
drum = cycleFromSample S.drum_0__Sample

drum_0 :: forall event. Cycle (Maybe (Note event))
drum_0 = cycleFromSample S.drum_0__Sample

drum_1 :: forall event. Cycle (Maybe (Note event))
drum_1 = cycleFromSample S.drum_1__Sample

drum_2 :: forall event. Cycle (Maybe (Note event))
drum_2 = cycleFromSample S.drum_2__Sample

drum_3 :: forall event. Cycle (Maybe (Note event))
drum_3 = cycleFromSample S.drum_3__Sample

drum_4 :: forall event. Cycle (Maybe (Note event))
drum_4 = cycleFromSample S.drum_4__Sample

drum_5 :: forall event. Cycle (Maybe (Note event))
drum_5 = cycleFromSample S.drum_5__Sample

coins :: forall event. Cycle (Maybe (Note event))
coins = cycleFromSample S.coins_0__Sample

coins_0 :: forall event. Cycle (Maybe (Note event))
coins_0 = cycleFromSample S.coins_0__Sample

industrial :: forall event. Cycle (Maybe (Note event))
industrial = cycleFromSample S.industrial_0__Sample

industrial_0 :: forall event. Cycle (Maybe (Note event))
industrial_0 = cycleFromSample S.industrial_0__Sample

industrial_1 :: forall event. Cycle (Maybe (Note event))
industrial_1 = cycleFromSample S.industrial_1__Sample

industrial_2 :: forall event. Cycle (Maybe (Note event))
industrial_2 = cycleFromSample S.industrial_2__Sample

industrial_3 :: forall event. Cycle (Maybe (Note event))
industrial_3 = cycleFromSample S.industrial_3__Sample

industrial_4 :: forall event. Cycle (Maybe (Note event))
industrial_4 = cycleFromSample S.industrial_4__Sample

industrial_5 :: forall event. Cycle (Maybe (Note event))
industrial_5 = cycleFromSample S.industrial_5__Sample

industrial_6 :: forall event. Cycle (Maybe (Note event))
industrial_6 = cycleFromSample S.industrial_6__Sample

industrial_7 :: forall event. Cycle (Maybe (Note event))
industrial_7 = cycleFromSample S.industrial_7__Sample

industrial_8 :: forall event. Cycle (Maybe (Note event))
industrial_8 = cycleFromSample S.industrial_8__Sample

industrial_9 :: forall event. Cycle (Maybe (Note event))
industrial_9 = cycleFromSample S.industrial_9__Sample

industrial_10 :: forall event. Cycle (Maybe (Note event))
industrial_10 = cycleFromSample S.industrial_10__Sample

industrial_11 :: forall event. Cycle (Maybe (Note event))
industrial_11 = cycleFromSample S.industrial_11__Sample

industrial_12 :: forall event. Cycle (Maybe (Note event))
industrial_12 = cycleFromSample S.industrial_12__Sample

industrial_13 :: forall event. Cycle (Maybe (Note event))
industrial_13 = cycleFromSample S.industrial_13__Sample

industrial_14 :: forall event. Cycle (Maybe (Note event))
industrial_14 = cycleFromSample S.industrial_14__Sample

industrial_15 :: forall event. Cycle (Maybe (Note event))
industrial_15 = cycleFromSample S.industrial_15__Sample

industrial_16 :: forall event. Cycle (Maybe (Note event))
industrial_16 = cycleFromSample S.industrial_16__Sample

industrial_17 :: forall event. Cycle (Maybe (Note event))
industrial_17 = cycleFromSample S.industrial_17__Sample

industrial_18 :: forall event. Cycle (Maybe (Note event))
industrial_18 = cycleFromSample S.industrial_18__Sample

industrial_19 :: forall event. Cycle (Maybe (Note event))
industrial_19 = cycleFromSample S.industrial_19__Sample

industrial_20 :: forall event. Cycle (Maybe (Note event))
industrial_20 = cycleFromSample S.industrial_20__Sample

industrial_21 :: forall event. Cycle (Maybe (Note event))
industrial_21 = cycleFromSample S.industrial_21__Sample

industrial_22 :: forall event. Cycle (Maybe (Note event))
industrial_22 = cycleFromSample S.industrial_22__Sample

industrial_23 :: forall event. Cycle (Maybe (Note event))
industrial_23 = cycleFromSample S.industrial_23__Sample

industrial_24 :: forall event. Cycle (Maybe (Note event))
industrial_24 = cycleFromSample S.industrial_24__Sample

industrial_25 :: forall event. Cycle (Maybe (Note event))
industrial_25 = cycleFromSample S.industrial_25__Sample

industrial_26 :: forall event. Cycle (Maybe (Note event))
industrial_26 = cycleFromSample S.industrial_26__Sample

industrial_27 :: forall event. Cycle (Maybe (Note event))
industrial_27 = cycleFromSample S.industrial_27__Sample

industrial_28 :: forall event. Cycle (Maybe (Note event))
industrial_28 = cycleFromSample S.industrial_28__Sample

industrial_29 :: forall event. Cycle (Maybe (Note event))
industrial_29 = cycleFromSample S.industrial_29__Sample

industrial_30 :: forall event. Cycle (Maybe (Note event))
industrial_30 = cycleFromSample S.industrial_30__Sample

industrial_31 :: forall event. Cycle (Maybe (Note event))
industrial_31 = cycleFromSample S.industrial_31__Sample

tink :: forall event. Cycle (Maybe (Note event))
tink = cycleFromSample S.tink_0__Sample

tink_0 :: forall event. Cycle (Maybe (Note event))
tink_0 = cycleFromSample S.tink_0__Sample

tink_1 :: forall event. Cycle (Maybe (Note event))
tink_1 = cycleFromSample S.tink_1__Sample

tink_2 :: forall event. Cycle (Maybe (Note event))
tink_2 = cycleFromSample S.tink_2__Sample

tink_3 :: forall event. Cycle (Maybe (Note event))
tink_3 = cycleFromSample S.tink_3__Sample

tink_4 :: forall event. Cycle (Maybe (Note event))
tink_4 = cycleFromSample S.tink_4__Sample

co :: forall event. Cycle (Maybe (Note event))
co = cycleFromSample S.co_0__Sample

co_0 :: forall event. Cycle (Maybe (Note event))
co_0 = cycleFromSample S.co_0__Sample

co_1 :: forall event. Cycle (Maybe (Note event))
co_1 = cycleFromSample S.co_1__Sample

co_2 :: forall event. Cycle (Maybe (Note event))
co_2 = cycleFromSample S.co_2__Sample

co_3 :: forall event. Cycle (Maybe (Note event))
co_3 = cycleFromSample S.co_3__Sample

fest :: forall event. Cycle (Maybe (Note event))
fest = cycleFromSample S.fest_0__Sample

fest_0 :: forall event. Cycle (Maybe (Note event))
fest_0 = cycleFromSample S.fest_0__Sample

feelfx :: forall event. Cycle (Maybe (Note event))
feelfx = cycleFromSample S.feelfx_0__Sample

feelfx_0 :: forall event. Cycle (Maybe (Note event))
feelfx_0 = cycleFromSample S.feelfx_0__Sample

feelfx_1 :: forall event. Cycle (Maybe (Note event))
feelfx_1 = cycleFromSample S.feelfx_1__Sample

feelfx_2 :: forall event. Cycle (Maybe (Note event))
feelfx_2 = cycleFromSample S.feelfx_2__Sample

feelfx_3 :: forall event. Cycle (Maybe (Note event))
feelfx_3 = cycleFromSample S.feelfx_3__Sample

feelfx_4 :: forall event. Cycle (Maybe (Note event))
feelfx_4 = cycleFromSample S.feelfx_4__Sample

feelfx_5 :: forall event. Cycle (Maybe (Note event))
feelfx_5 = cycleFromSample S.feelfx_5__Sample

feelfx_6 :: forall event. Cycle (Maybe (Note event))
feelfx_6 = cycleFromSample S.feelfx_6__Sample

feelfx_7 :: forall event. Cycle (Maybe (Note event))
feelfx_7 = cycleFromSample S.feelfx_7__Sample

x_808cy :: forall event. Cycle (Maybe (Note event))
x_808cy = cycleFromSample S.x_808cy_0__Sample

x_808cy_0 :: forall event. Cycle (Maybe (Note event))
x_808cy_0 = cycleFromSample S.x_808cy_0__Sample

x_808cy_1 :: forall event. Cycle (Maybe (Note event))
x_808cy_1 = cycleFromSample S.x_808cy_1__Sample

x_808cy_2 :: forall event. Cycle (Maybe (Note event))
x_808cy_2 = cycleFromSample S.x_808cy_2__Sample

x_808cy_3 :: forall event. Cycle (Maybe (Note event))
x_808cy_3 = cycleFromSample S.x_808cy_3__Sample

x_808cy_4 :: forall event. Cycle (Maybe (Note event))
x_808cy_4 = cycleFromSample S.x_808cy_4__Sample

x_808cy_5 :: forall event. Cycle (Maybe (Note event))
x_808cy_5 = cycleFromSample S.x_808cy_5__Sample

x_808cy_6 :: forall event. Cycle (Maybe (Note event))
x_808cy_6 = cycleFromSample S.x_808cy_6__Sample

x_808cy_7 :: forall event. Cycle (Maybe (Note event))
x_808cy_7 = cycleFromSample S.x_808cy_7__Sample

x_808cy_8 :: forall event. Cycle (Maybe (Note event))
x_808cy_8 = cycleFromSample S.x_808cy_8__Sample

x_808cy_9 :: forall event. Cycle (Maybe (Note event))
x_808cy_9 = cycleFromSample S.x_808cy_9__Sample

x_808cy_10 :: forall event. Cycle (Maybe (Note event))
x_808cy_10 = cycleFromSample S.x_808cy_10__Sample

x_808cy_11 :: forall event. Cycle (Maybe (Note event))
x_808cy_11 = cycleFromSample S.x_808cy_11__Sample

x_808cy_12 :: forall event. Cycle (Maybe (Note event))
x_808cy_12 = cycleFromSample S.x_808cy_12__Sample

x_808cy_13 :: forall event. Cycle (Maybe (Note event))
x_808cy_13 = cycleFromSample S.x_808cy_13__Sample

x_808cy_14 :: forall event. Cycle (Maybe (Note event))
x_808cy_14 = cycleFromSample S.x_808cy_14__Sample

x_808cy_15 :: forall event. Cycle (Maybe (Note event))
x_808cy_15 = cycleFromSample S.x_808cy_15__Sample

x_808cy_16 :: forall event. Cycle (Maybe (Note event))
x_808cy_16 = cycleFromSample S.x_808cy_16__Sample

x_808cy_17 :: forall event. Cycle (Maybe (Note event))
x_808cy_17 = cycleFromSample S.x_808cy_17__Sample

x_808cy_18 :: forall event. Cycle (Maybe (Note event))
x_808cy_18 = cycleFromSample S.x_808cy_18__Sample

x_808cy_19 :: forall event. Cycle (Maybe (Note event))
x_808cy_19 = cycleFromSample S.x_808cy_19__Sample

x_808cy_20 :: forall event. Cycle (Maybe (Note event))
x_808cy_20 = cycleFromSample S.x_808cy_20__Sample

x_808cy_21 :: forall event. Cycle (Maybe (Note event))
x_808cy_21 = cycleFromSample S.x_808cy_21__Sample

x_808cy_22 :: forall event. Cycle (Maybe (Note event))
x_808cy_22 = cycleFromSample S.x_808cy_22__Sample

x_808cy_23 :: forall event. Cycle (Maybe (Note event))
x_808cy_23 = cycleFromSample S.x_808cy_23__Sample

x_808cy_24 :: forall event. Cycle (Maybe (Note event))
x_808cy_24 = cycleFromSample S.x_808cy_24__Sample

world :: forall event. Cycle (Maybe (Note event))
world = cycleFromSample S.world_0__Sample

world_0 :: forall event. Cycle (Maybe (Note event))
world_0 = cycleFromSample S.world_0__Sample

world_1 :: forall event. Cycle (Maybe (Note event))
world_1 = cycleFromSample S.world_1__Sample

world_2 :: forall event. Cycle (Maybe (Note event))
world_2 = cycleFromSample S.world_2__Sample

f :: forall event. Cycle (Maybe (Note event))
f = cycleFromSample S.f_0__Sample

f_0 :: forall event. Cycle (Maybe (Note event))
f_0 = cycleFromSample S.f_0__Sample

numbers :: forall event. Cycle (Maybe (Note event))
numbers = cycleFromSample S.numbers_0__Sample

numbers_0 :: forall event. Cycle (Maybe (Note event))
numbers_0 = cycleFromSample S.numbers_0__Sample

numbers_1 :: forall event. Cycle (Maybe (Note event))
numbers_1 = cycleFromSample S.numbers_1__Sample

numbers_2 :: forall event. Cycle (Maybe (Note event))
numbers_2 = cycleFromSample S.numbers_2__Sample

numbers_3 :: forall event. Cycle (Maybe (Note event))
numbers_3 = cycleFromSample S.numbers_3__Sample

numbers_4 :: forall event. Cycle (Maybe (Note event))
numbers_4 = cycleFromSample S.numbers_4__Sample

numbers_5 :: forall event. Cycle (Maybe (Note event))
numbers_5 = cycleFromSample S.numbers_5__Sample

numbers_6 :: forall event. Cycle (Maybe (Note event))
numbers_6 = cycleFromSample S.numbers_6__Sample

numbers_7 :: forall event. Cycle (Maybe (Note event))
numbers_7 = cycleFromSample S.numbers_7__Sample

numbers_8 :: forall event. Cycle (Maybe (Note event))
numbers_8 = cycleFromSample S.numbers_8__Sample

d :: forall event. Cycle (Maybe (Note event))
d = cycleFromSample S.d_0__Sample

d_0 :: forall event. Cycle (Maybe (Note event))
d_0 = cycleFromSample S.d_0__Sample

d_1 :: forall event. Cycle (Maybe (Note event))
d_1 = cycleFromSample S.d_1__Sample

d_2 :: forall event. Cycle (Maybe (Note event))
d_2 = cycleFromSample S.d_2__Sample

d_3 :: forall event. Cycle (Maybe (Note event))
d_3 = cycleFromSample S.d_3__Sample

padlong :: forall event. Cycle (Maybe (Note event))
padlong = cycleFromSample S.padlong_0__Sample

padlong_0 :: forall event. Cycle (Maybe (Note event))
padlong_0 = cycleFromSample S.padlong_0__Sample

sequential :: forall event. Cycle (Maybe (Note event))
sequential = cycleFromSample S.sequential_0__Sample

sequential_0 :: forall event. Cycle (Maybe (Note event))
sequential_0 = cycleFromSample S.sequential_0__Sample

sequential_1 :: forall event. Cycle (Maybe (Note event))
sequential_1 = cycleFromSample S.sequential_1__Sample

sequential_2 :: forall event. Cycle (Maybe (Note event))
sequential_2 = cycleFromSample S.sequential_2__Sample

sequential_3 :: forall event. Cycle (Maybe (Note event))
sequential_3 = cycleFromSample S.sequential_3__Sample

sequential_4 :: forall event. Cycle (Maybe (Note event))
sequential_4 = cycleFromSample S.sequential_4__Sample

sequential_5 :: forall event. Cycle (Maybe (Note event))
sequential_5 = cycleFromSample S.sequential_5__Sample

sequential_6 :: forall event. Cycle (Maybe (Note event))
sequential_6 = cycleFromSample S.sequential_6__Sample

sequential_7 :: forall event. Cycle (Maybe (Note event))
sequential_7 = cycleFromSample S.sequential_7__Sample

stab :: forall event. Cycle (Maybe (Note event))
stab = cycleFromSample S.stab_0__Sample

stab_0 :: forall event. Cycle (Maybe (Note event))
stab_0 = cycleFromSample S.stab_0__Sample

stab_1 :: forall event. Cycle (Maybe (Note event))
stab_1 = cycleFromSample S.stab_1__Sample

stab_2 :: forall event. Cycle (Maybe (Note event))
stab_2 = cycleFromSample S.stab_2__Sample

stab_3 :: forall event. Cycle (Maybe (Note event))
stab_3 = cycleFromSample S.stab_3__Sample

stab_4 :: forall event. Cycle (Maybe (Note event))
stab_4 = cycleFromSample S.stab_4__Sample

stab_5 :: forall event. Cycle (Maybe (Note event))
stab_5 = cycleFromSample S.stab_5__Sample

stab_6 :: forall event. Cycle (Maybe (Note event))
stab_6 = cycleFromSample S.stab_6__Sample

stab_7 :: forall event. Cycle (Maybe (Note event))
stab_7 = cycleFromSample S.stab_7__Sample

stab_8 :: forall event. Cycle (Maybe (Note event))
stab_8 = cycleFromSample S.stab_8__Sample

stab_9 :: forall event. Cycle (Maybe (Note event))
stab_9 = cycleFromSample S.stab_9__Sample

stab_10 :: forall event. Cycle (Maybe (Note event))
stab_10 = cycleFromSample S.stab_10__Sample

stab_11 :: forall event. Cycle (Maybe (Note event))
stab_11 = cycleFromSample S.stab_11__Sample

stab_12 :: forall event. Cycle (Maybe (Note event))
stab_12 = cycleFromSample S.stab_12__Sample

stab_13 :: forall event. Cycle (Maybe (Note event))
stab_13 = cycleFromSample S.stab_13__Sample

stab_14 :: forall event. Cycle (Maybe (Note event))
stab_14 = cycleFromSample S.stab_14__Sample

stab_15 :: forall event. Cycle (Maybe (Note event))
stab_15 = cycleFromSample S.stab_15__Sample

stab_16 :: forall event. Cycle (Maybe (Note event))
stab_16 = cycleFromSample S.stab_16__Sample

stab_17 :: forall event. Cycle (Maybe (Note event))
stab_17 = cycleFromSample S.stab_17__Sample

stab_18 :: forall event. Cycle (Maybe (Note event))
stab_18 = cycleFromSample S.stab_18__Sample

stab_19 :: forall event. Cycle (Maybe (Note event))
stab_19 = cycleFromSample S.stab_19__Sample

stab_20 :: forall event. Cycle (Maybe (Note event))
stab_20 = cycleFromSample S.stab_20__Sample

stab_21 :: forall event. Cycle (Maybe (Note event))
stab_21 = cycleFromSample S.stab_21__Sample

stab_22 :: forall event. Cycle (Maybe (Note event))
stab_22 = cycleFromSample S.stab_22__Sample

electro1 :: forall event. Cycle (Maybe (Note event))
electro1 = cycleFromSample S.electro1_0__Sample

electro1_0 :: forall event. Cycle (Maybe (Note event))
electro1_0 = cycleFromSample S.electro1_0__Sample

electro1_1 :: forall event. Cycle (Maybe (Note event))
electro1_1 = cycleFromSample S.electro1_1__Sample

electro1_2 :: forall event. Cycle (Maybe (Note event))
electro1_2 = cycleFromSample S.electro1_2__Sample

electro1_3 :: forall event. Cycle (Maybe (Note event))
electro1_3 = cycleFromSample S.electro1_3__Sample

electro1_4 :: forall event. Cycle (Maybe (Note event))
electro1_4 = cycleFromSample S.electro1_4__Sample

electro1_5 :: forall event. Cycle (Maybe (Note event))
electro1_5 = cycleFromSample S.electro1_5__Sample

electro1_6 :: forall event. Cycle (Maybe (Note event))
electro1_6 = cycleFromSample S.electro1_6__Sample

electro1_7 :: forall event. Cycle (Maybe (Note event))
electro1_7 = cycleFromSample S.electro1_7__Sample

electro1_8 :: forall event. Cycle (Maybe (Note event))
electro1_8 = cycleFromSample S.electro1_8__Sample

electro1_9 :: forall event. Cycle (Maybe (Note event))
electro1_9 = cycleFromSample S.electro1_9__Sample

electro1_10 :: forall event. Cycle (Maybe (Note event))
electro1_10 = cycleFromSample S.electro1_10__Sample

electro1_11 :: forall event. Cycle (Maybe (Note event))
electro1_11 = cycleFromSample S.electro1_11__Sample

electro1_12 :: forall event. Cycle (Maybe (Note event))
electro1_12 = cycleFromSample S.electro1_12__Sample

ifdrums :: forall event. Cycle (Maybe (Note event))
ifdrums = cycleFromSample S.ifdrums_0__Sample

ifdrums_0 :: forall event. Cycle (Maybe (Note event))
ifdrums_0 = cycleFromSample S.ifdrums_0__Sample

ifdrums_1 :: forall event. Cycle (Maybe (Note event))
ifdrums_1 = cycleFromSample S.ifdrums_1__Sample

ifdrums_2 :: forall event. Cycle (Maybe (Note event))
ifdrums_2 = cycleFromSample S.ifdrums_2__Sample

invaders :: forall event. Cycle (Maybe (Note event))
invaders = cycleFromSample S.invaders_0__Sample

invaders_0 :: forall event. Cycle (Maybe (Note event))
invaders_0 = cycleFromSample S.invaders_0__Sample

invaders_1 :: forall event. Cycle (Maybe (Note event))
invaders_1 = cycleFromSample S.invaders_1__Sample

invaders_2 :: forall event. Cycle (Maybe (Note event))
invaders_2 = cycleFromSample S.invaders_2__Sample

invaders_3 :: forall event. Cycle (Maybe (Note event))
invaders_3 = cycleFromSample S.invaders_3__Sample

invaders_4 :: forall event. Cycle (Maybe (Note event))
invaders_4 = cycleFromSample S.invaders_4__Sample

invaders_5 :: forall event. Cycle (Maybe (Note event))
invaders_5 = cycleFromSample S.invaders_5__Sample

invaders_6 :: forall event. Cycle (Maybe (Note event))
invaders_6 = cycleFromSample S.invaders_6__Sample

invaders_7 :: forall event. Cycle (Maybe (Note event))
invaders_7 = cycleFromSample S.invaders_7__Sample

invaders_8 :: forall event. Cycle (Maybe (Note event))
invaders_8 = cycleFromSample S.invaders_8__Sample

invaders_9 :: forall event. Cycle (Maybe (Note event))
invaders_9 = cycleFromSample S.invaders_9__Sample

invaders_10 :: forall event. Cycle (Maybe (Note event))
invaders_10 = cycleFromSample S.invaders_10__Sample

invaders_11 :: forall event. Cycle (Maybe (Note event))
invaders_11 = cycleFromSample S.invaders_11__Sample

invaders_12 :: forall event. Cycle (Maybe (Note event))
invaders_12 = cycleFromSample S.invaders_12__Sample

invaders_13 :: forall event. Cycle (Maybe (Note event))
invaders_13 = cycleFromSample S.invaders_13__Sample

invaders_14 :: forall event. Cycle (Maybe (Note event))
invaders_14 = cycleFromSample S.invaders_14__Sample

invaders_15 :: forall event. Cycle (Maybe (Note event))
invaders_15 = cycleFromSample S.invaders_15__Sample

invaders_16 :: forall event. Cycle (Maybe (Note event))
invaders_16 = cycleFromSample S.invaders_16__Sample

invaders_17 :: forall event. Cycle (Maybe (Note event))
invaders_17 = cycleFromSample S.invaders_17__Sample

dist :: forall event. Cycle (Maybe (Note event))
dist = cycleFromSample S.dist_0__Sample

dist_0 :: forall event. Cycle (Maybe (Note event))
dist_0 = cycleFromSample S.dist_0__Sample

dist_1 :: forall event. Cycle (Maybe (Note event))
dist_1 = cycleFromSample S.dist_1__Sample

dist_2 :: forall event. Cycle (Maybe (Note event))
dist_2 = cycleFromSample S.dist_2__Sample

dist_3 :: forall event. Cycle (Maybe (Note event))
dist_3 = cycleFromSample S.dist_3__Sample

dist_4 :: forall event. Cycle (Maybe (Note event))
dist_4 = cycleFromSample S.dist_4__Sample

dist_5 :: forall event. Cycle (Maybe (Note event))
dist_5 = cycleFromSample S.dist_5__Sample

dist_6 :: forall event. Cycle (Maybe (Note event))
dist_6 = cycleFromSample S.dist_6__Sample

dist_7 :: forall event. Cycle (Maybe (Note event))
dist_7 = cycleFromSample S.dist_7__Sample

dist_8 :: forall event. Cycle (Maybe (Note event))
dist_8 = cycleFromSample S.dist_8__Sample

dist_9 :: forall event. Cycle (Maybe (Note event))
dist_9 = cycleFromSample S.dist_9__Sample

dist_10 :: forall event. Cycle (Maybe (Note event))
dist_10 = cycleFromSample S.dist_10__Sample

dist_11 :: forall event. Cycle (Maybe (Note event))
dist_11 = cycleFromSample S.dist_11__Sample

dist_12 :: forall event. Cycle (Maybe (Note event))
dist_12 = cycleFromSample S.dist_12__Sample

dist_13 :: forall event. Cycle (Maybe (Note event))
dist_13 = cycleFromSample S.dist_13__Sample

dist_14 :: forall event. Cycle (Maybe (Note event))
dist_14 = cycleFromSample S.dist_14__Sample

dist_15 :: forall event. Cycle (Maybe (Note event))
dist_15 = cycleFromSample S.dist_15__Sample

sundance :: forall event. Cycle (Maybe (Note event))
sundance = cycleFromSample S.sundance_0__Sample

sundance_0 :: forall event. Cycle (Maybe (Note event))
sundance_0 = cycleFromSample S.sundance_0__Sample

sundance_1 :: forall event. Cycle (Maybe (Note event))
sundance_1 = cycleFromSample S.sundance_1__Sample

sundance_2 :: forall event. Cycle (Maybe (Note event))
sundance_2 = cycleFromSample S.sundance_2__Sample

sundance_3 :: forall event. Cycle (Maybe (Note event))
sundance_3 = cycleFromSample S.sundance_3__Sample

sundance_4 :: forall event. Cycle (Maybe (Note event))
sundance_4 = cycleFromSample S.sundance_4__Sample

sundance_5 :: forall event. Cycle (Maybe (Note event))
sundance_5 = cycleFromSample S.sundance_5__Sample

speech :: forall event. Cycle (Maybe (Note event))
speech = cycleFromSample S.speech_0__Sample

speech_0 :: forall event. Cycle (Maybe (Note event))
speech_0 = cycleFromSample S.speech_0__Sample

speech_1 :: forall event. Cycle (Maybe (Note event))
speech_1 = cycleFromSample S.speech_1__Sample

speech_2 :: forall event. Cycle (Maybe (Note event))
speech_2 = cycleFromSample S.speech_2__Sample

speech_3 :: forall event. Cycle (Maybe (Note event))
speech_3 = cycleFromSample S.speech_3__Sample

speech_4 :: forall event. Cycle (Maybe (Note event))
speech_4 = cycleFromSample S.speech_4__Sample

speech_5 :: forall event. Cycle (Maybe (Note event))
speech_5 = cycleFromSample S.speech_5__Sample

speech_6 :: forall event. Cycle (Maybe (Note event))
speech_6 = cycleFromSample S.speech_6__Sample

toys :: forall event. Cycle (Maybe (Note event))
toys = cycleFromSample S.toys_0__Sample

toys_0 :: forall event. Cycle (Maybe (Note event))
toys_0 = cycleFromSample S.toys_0__Sample

toys_1 :: forall event. Cycle (Maybe (Note event))
toys_1 = cycleFromSample S.toys_1__Sample

toys_2 :: forall event. Cycle (Maybe (Note event))
toys_2 = cycleFromSample S.toys_2__Sample

toys_3 :: forall event. Cycle (Maybe (Note event))
toys_3 = cycleFromSample S.toys_3__Sample

toys_4 :: forall event. Cycle (Maybe (Note event))
toys_4 = cycleFromSample S.toys_4__Sample

toys_5 :: forall event. Cycle (Maybe (Note event))
toys_5 = cycleFromSample S.toys_5__Sample

toys_6 :: forall event. Cycle (Maybe (Note event))
toys_6 = cycleFromSample S.toys_6__Sample

toys_7 :: forall event. Cycle (Maybe (Note event))
toys_7 = cycleFromSample S.toys_7__Sample

toys_8 :: forall event. Cycle (Maybe (Note event))
toys_8 = cycleFromSample S.toys_8__Sample

toys_9 :: forall event. Cycle (Maybe (Note event))
toys_9 = cycleFromSample S.toys_9__Sample

toys_10 :: forall event. Cycle (Maybe (Note event))
toys_10 = cycleFromSample S.toys_10__Sample

toys_11 :: forall event. Cycle (Maybe (Note event))
toys_11 = cycleFromSample S.toys_11__Sample

toys_12 :: forall event. Cycle (Maybe (Note event))
toys_12 = cycleFromSample S.toys_12__Sample

bass0 :: forall event. Cycle (Maybe (Note event))
bass0 = cycleFromSample S.bass0_0__Sample

bass0_0 :: forall event. Cycle (Maybe (Note event))
bass0_0 = cycleFromSample S.bass0_0__Sample

bass0_1 :: forall event. Cycle (Maybe (Note event))
bass0_1 = cycleFromSample S.bass0_1__Sample

bass0_2 :: forall event. Cycle (Maybe (Note event))
bass0_2 = cycleFromSample S.bass0_2__Sample

realclaps :: forall event. Cycle (Maybe (Note event))
realclaps = cycleFromSample S.realclaps_0__Sample

realclaps_0 :: forall event. Cycle (Maybe (Note event))
realclaps_0 = cycleFromSample S.realclaps_0__Sample

realclaps_1 :: forall event. Cycle (Maybe (Note event))
realclaps_1 = cycleFromSample S.realclaps_1__Sample

realclaps_2 :: forall event. Cycle (Maybe (Note event))
realclaps_2 = cycleFromSample S.realclaps_2__Sample

realclaps_3 :: forall event. Cycle (Maybe (Note event))
realclaps_3 = cycleFromSample S.realclaps_3__Sample

dorkbot :: forall event. Cycle (Maybe (Note event))
dorkbot = cycleFromSample S.dorkbot_0__Sample

dorkbot_0 :: forall event. Cycle (Maybe (Note event))
dorkbot_0 = cycleFromSample S.dorkbot_0__Sample

dorkbot_1 :: forall event. Cycle (Maybe (Note event))
dorkbot_1 = cycleFromSample S.dorkbot_1__Sample

arpy :: forall event. Cycle (Maybe (Note event))
arpy = cycleFromSample S.arpy_0__Sample

arpy_0 :: forall event. Cycle (Maybe (Note event))
arpy_0 = cycleFromSample S.arpy_0__Sample

arpy_1 :: forall event. Cycle (Maybe (Note event))
arpy_1 = cycleFromSample S.arpy_1__Sample

arpy_2 :: forall event. Cycle (Maybe (Note event))
arpy_2 = cycleFromSample S.arpy_2__Sample

arpy_3 :: forall event. Cycle (Maybe (Note event))
arpy_3 = cycleFromSample S.arpy_3__Sample

arpy_4 :: forall event. Cycle (Maybe (Note event))
arpy_4 = cycleFromSample S.arpy_4__Sample

arpy_5 :: forall event. Cycle (Maybe (Note event))
arpy_5 = cycleFromSample S.arpy_5__Sample

arpy_6 :: forall event. Cycle (Maybe (Note event))
arpy_6 = cycleFromSample S.arpy_6__Sample

arpy_7 :: forall event. Cycle (Maybe (Note event))
arpy_7 = cycleFromSample S.arpy_7__Sample

arpy_8 :: forall event. Cycle (Maybe (Note event))
arpy_8 = cycleFromSample S.arpy_8__Sample

arpy_9 :: forall event. Cycle (Maybe (Note event))
arpy_9 = cycleFromSample S.arpy_9__Sample

arpy_10 :: forall event. Cycle (Maybe (Note event))
arpy_10 = cycleFromSample S.arpy_10__Sample

fire :: forall event. Cycle (Maybe (Note event))
fire = cycleFromSample S.fire_0__Sample

fire_0 :: forall event. Cycle (Maybe (Note event))
fire_0 = cycleFromSample S.fire_0__Sample

hoover :: forall event. Cycle (Maybe (Note event))
hoover = cycleFromSample S.hoover_0__Sample

hoover_0 :: forall event. Cycle (Maybe (Note event))
hoover_0 = cycleFromSample S.hoover_0__Sample

hoover_1 :: forall event. Cycle (Maybe (Note event))
hoover_1 = cycleFromSample S.hoover_1__Sample

hoover_2 :: forall event. Cycle (Maybe (Note event))
hoover_2 = cycleFromSample S.hoover_2__Sample

hoover_3 :: forall event. Cycle (Maybe (Note event))
hoover_3 = cycleFromSample S.hoover_3__Sample

hoover_4 :: forall event. Cycle (Maybe (Note event))
hoover_4 = cycleFromSample S.hoover_4__Sample

hoover_5 :: forall event. Cycle (Maybe (Note event))
hoover_5 = cycleFromSample S.hoover_5__Sample

breath :: forall event. Cycle (Maybe (Note event))
breath = cycleFromSample S.breath_0__Sample

breath_0 :: forall event. Cycle (Maybe (Note event))
breath_0 = cycleFromSample S.breath_0__Sample

rave :: forall event. Cycle (Maybe (Note event))
rave = cycleFromSample S.rave_0__Sample

rave_0 :: forall event. Cycle (Maybe (Note event))
rave_0 = cycleFromSample S.rave_0__Sample

rave_1 :: forall event. Cycle (Maybe (Note event))
rave_1 = cycleFromSample S.rave_1__Sample

rave_2 :: forall event. Cycle (Maybe (Note event))
rave_2 = cycleFromSample S.rave_2__Sample

rave_3 :: forall event. Cycle (Maybe (Note event))
rave_3 = cycleFromSample S.rave_3__Sample

rave_4 :: forall event. Cycle (Maybe (Note event))
rave_4 = cycleFromSample S.rave_4__Sample

rave_5 :: forall event. Cycle (Maybe (Note event))
rave_5 = cycleFromSample S.rave_5__Sample

rave_6 :: forall event. Cycle (Maybe (Note event))
rave_6 = cycleFromSample S.rave_6__Sample

rave_7 :: forall event. Cycle (Maybe (Note event))
rave_7 = cycleFromSample S.rave_7__Sample

bottle :: forall event. Cycle (Maybe (Note event))
bottle = cycleFromSample S.bottle_0__Sample

bottle_0 :: forall event. Cycle (Maybe (Note event))
bottle_0 = cycleFromSample S.bottle_0__Sample

bottle_1 :: forall event. Cycle (Maybe (Note event))
bottle_1 = cycleFromSample S.bottle_1__Sample

bottle_2 :: forall event. Cycle (Maybe (Note event))
bottle_2 = cycleFromSample S.bottle_2__Sample

bottle_3 :: forall event. Cycle (Maybe (Note event))
bottle_3 = cycleFromSample S.bottle_3__Sample

bottle_4 :: forall event. Cycle (Maybe (Note event))
bottle_4 = cycleFromSample S.bottle_4__Sample

bottle_5 :: forall event. Cycle (Maybe (Note event))
bottle_5 = cycleFromSample S.bottle_5__Sample

bottle_6 :: forall event. Cycle (Maybe (Note event))
bottle_6 = cycleFromSample S.bottle_6__Sample

bottle_7 :: forall event. Cycle (Maybe (Note event))
bottle_7 = cycleFromSample S.bottle_7__Sample

bottle_8 :: forall event. Cycle (Maybe (Note event))
bottle_8 = cycleFromSample S.bottle_8__Sample

bottle_9 :: forall event. Cycle (Maybe (Note event))
bottle_9 = cycleFromSample S.bottle_9__Sample

bottle_10 :: forall event. Cycle (Maybe (Note event))
bottle_10 = cycleFromSample S.bottle_10__Sample

bottle_11 :: forall event. Cycle (Maybe (Note event))
bottle_11 = cycleFromSample S.bottle_11__Sample

bottle_12 :: forall event. Cycle (Maybe (Note event))
bottle_12 = cycleFromSample S.bottle_12__Sample

east :: forall event. Cycle (Maybe (Note event))
east = cycleFromSample S.east_0__Sample

east_0 :: forall event. Cycle (Maybe (Note event))
east_0 = cycleFromSample S.east_0__Sample

east_1 :: forall event. Cycle (Maybe (Note event))
east_1 = cycleFromSample S.east_1__Sample

east_2 :: forall event. Cycle (Maybe (Note event))
east_2 = cycleFromSample S.east_2__Sample

east_3 :: forall event. Cycle (Maybe (Note event))
east_3 = cycleFromSample S.east_3__Sample

east_4 :: forall event. Cycle (Maybe (Note event))
east_4 = cycleFromSample S.east_4__Sample

east_5 :: forall event. Cycle (Maybe (Note event))
east_5 = cycleFromSample S.east_5__Sample

east_6 :: forall event. Cycle (Maybe (Note event))
east_6 = cycleFromSample S.east_6__Sample

east_7 :: forall event. Cycle (Maybe (Note event))
east_7 = cycleFromSample S.east_7__Sample

east_8 :: forall event. Cycle (Maybe (Note event))
east_8 = cycleFromSample S.east_8__Sample

linnhats :: forall event. Cycle (Maybe (Note event))
linnhats = cycleFromSample S.linnhats_0__Sample

linnhats_0 :: forall event. Cycle (Maybe (Note event))
linnhats_0 = cycleFromSample S.linnhats_0__Sample

linnhats_1 :: forall event. Cycle (Maybe (Note event))
linnhats_1 = cycleFromSample S.linnhats_1__Sample

linnhats_2 :: forall event. Cycle (Maybe (Note event))
linnhats_2 = cycleFromSample S.linnhats_2__Sample

linnhats_3 :: forall event. Cycle (Maybe (Note event))
linnhats_3 = cycleFromSample S.linnhats_3__Sample

linnhats_4 :: forall event. Cycle (Maybe (Note event))
linnhats_4 = cycleFromSample S.linnhats_4__Sample

linnhats_5 :: forall event. Cycle (Maybe (Note event))
linnhats_5 = cycleFromSample S.linnhats_5__Sample

speedupdown :: forall event. Cycle (Maybe (Note event))
speedupdown = cycleFromSample S.speedupdown_0__Sample

speedupdown_0 :: forall event. Cycle (Maybe (Note event))
speedupdown_0 = cycleFromSample S.speedupdown_0__Sample

speedupdown_1 :: forall event. Cycle (Maybe (Note event))
speedupdown_1 = cycleFromSample S.speedupdown_1__Sample

speedupdown_2 :: forall event. Cycle (Maybe (Note event))
speedupdown_2 = cycleFromSample S.speedupdown_2__Sample

speedupdown_3 :: forall event. Cycle (Maybe (Note event))
speedupdown_3 = cycleFromSample S.speedupdown_3__Sample

speedupdown_4 :: forall event. Cycle (Maybe (Note event))
speedupdown_4 = cycleFromSample S.speedupdown_4__Sample

speedupdown_5 :: forall event. Cycle (Maybe (Note event))
speedupdown_5 = cycleFromSample S.speedupdown_5__Sample

speedupdown_6 :: forall event. Cycle (Maybe (Note event))
speedupdown_6 = cycleFromSample S.speedupdown_6__Sample

speedupdown_7 :: forall event. Cycle (Maybe (Note event))
speedupdown_7 = cycleFromSample S.speedupdown_7__Sample

speedupdown_8 :: forall event. Cycle (Maybe (Note event))
speedupdown_8 = cycleFromSample S.speedupdown_8__Sample

cosmicg :: forall event. Cycle (Maybe (Note event))
cosmicg = cycleFromSample S.cosmicg_0__Sample

cosmicg_0 :: forall event. Cycle (Maybe (Note event))
cosmicg_0 = cycleFromSample S.cosmicg_0__Sample

cosmicg_1 :: forall event. Cycle (Maybe (Note event))
cosmicg_1 = cycleFromSample S.cosmicg_1__Sample

cosmicg_2 :: forall event. Cycle (Maybe (Note event))
cosmicg_2 = cycleFromSample S.cosmicg_2__Sample

cosmicg_3 :: forall event. Cycle (Maybe (Note event))
cosmicg_3 = cycleFromSample S.cosmicg_3__Sample

cosmicg_4 :: forall event. Cycle (Maybe (Note event))
cosmicg_4 = cycleFromSample S.cosmicg_4__Sample

cosmicg_5 :: forall event. Cycle (Maybe (Note event))
cosmicg_5 = cycleFromSample S.cosmicg_5__Sample

cosmicg_6 :: forall event. Cycle (Maybe (Note event))
cosmicg_6 = cycleFromSample S.cosmicg_6__Sample

cosmicg_7 :: forall event. Cycle (Maybe (Note event))
cosmicg_7 = cycleFromSample S.cosmicg_7__Sample

cosmicg_8 :: forall event. Cycle (Maybe (Note event))
cosmicg_8 = cycleFromSample S.cosmicg_8__Sample

cosmicg_9 :: forall event. Cycle (Maybe (Note event))
cosmicg_9 = cycleFromSample S.cosmicg_9__Sample

cosmicg_10 :: forall event. Cycle (Maybe (Note event))
cosmicg_10 = cycleFromSample S.cosmicg_10__Sample

cosmicg_11 :: forall event. Cycle (Maybe (Note event))
cosmicg_11 = cycleFromSample S.cosmicg_11__Sample

cosmicg_12 :: forall event. Cycle (Maybe (Note event))
cosmicg_12 = cycleFromSample S.cosmicg_12__Sample

cosmicg_13 :: forall event. Cycle (Maybe (Note event))
cosmicg_13 = cycleFromSample S.cosmicg_13__Sample

cosmicg_14 :: forall event. Cycle (Maybe (Note event))
cosmicg_14 = cycleFromSample S.cosmicg_14__Sample

jvbass :: forall event. Cycle (Maybe (Note event))
jvbass = cycleFromSample S.jvbass_0__Sample

jvbass_0 :: forall event. Cycle (Maybe (Note event))
jvbass_0 = cycleFromSample S.jvbass_0__Sample

jvbass_1 :: forall event. Cycle (Maybe (Note event))
jvbass_1 = cycleFromSample S.jvbass_1__Sample

jvbass_2 :: forall event. Cycle (Maybe (Note event))
jvbass_2 = cycleFromSample S.jvbass_2__Sample

jvbass_3 :: forall event. Cycle (Maybe (Note event))
jvbass_3 = cycleFromSample S.jvbass_3__Sample

jvbass_4 :: forall event. Cycle (Maybe (Note event))
jvbass_4 = cycleFromSample S.jvbass_4__Sample

jvbass_5 :: forall event. Cycle (Maybe (Note event))
jvbass_5 = cycleFromSample S.jvbass_5__Sample

jvbass_6 :: forall event. Cycle (Maybe (Note event))
jvbass_6 = cycleFromSample S.jvbass_6__Sample

jvbass_7 :: forall event. Cycle (Maybe (Note event))
jvbass_7 = cycleFromSample S.jvbass_7__Sample

jvbass_8 :: forall event. Cycle (Maybe (Note event))
jvbass_8 = cycleFromSample S.jvbass_8__Sample

jvbass_9 :: forall event. Cycle (Maybe (Note event))
jvbass_9 = cycleFromSample S.jvbass_9__Sample

jvbass_10 :: forall event. Cycle (Maybe (Note event))
jvbass_10 = cycleFromSample S.jvbass_10__Sample

jvbass_11 :: forall event. Cycle (Maybe (Note event))
jvbass_11 = cycleFromSample S.jvbass_11__Sample

jvbass_12 :: forall event. Cycle (Maybe (Note event))
jvbass_12 = cycleFromSample S.jvbass_12__Sample

mash :: forall event. Cycle (Maybe (Note event))
mash = cycleFromSample S.mash_0__Sample

mash_0 :: forall event. Cycle (Maybe (Note event))
mash_0 = cycleFromSample S.mash_0__Sample

mash_1 :: forall event. Cycle (Maybe (Note event))
mash_1 = cycleFromSample S.mash_1__Sample

feel :: forall event. Cycle (Maybe (Note event))
feel = cycleFromSample S.feel_0__Sample

feel_0 :: forall event. Cycle (Maybe (Note event))
feel_0 = cycleFromSample S.feel_0__Sample

feel_1 :: forall event. Cycle (Maybe (Note event))
feel_1 = cycleFromSample S.feel_1__Sample

feel_2 :: forall event. Cycle (Maybe (Note event))
feel_2 = cycleFromSample S.feel_2__Sample

feel_3 :: forall event. Cycle (Maybe (Note event))
feel_3 = cycleFromSample S.feel_3__Sample

feel_4 :: forall event. Cycle (Maybe (Note event))
feel_4 = cycleFromSample S.feel_4__Sample

feel_5 :: forall event. Cycle (Maybe (Note event))
feel_5 = cycleFromSample S.feel_5__Sample

feel_6 :: forall event. Cycle (Maybe (Note event))
feel_6 = cycleFromSample S.feel_6__Sample

short :: forall event. Cycle (Maybe (Note event))
short = cycleFromSample S.short_0__Sample

short_0 :: forall event. Cycle (Maybe (Note event))
short_0 = cycleFromSample S.short_0__Sample

short_1 :: forall event. Cycle (Maybe (Note event))
short_1 = cycleFromSample S.short_1__Sample

short_2 :: forall event. Cycle (Maybe (Note event))
short_2 = cycleFromSample S.short_2__Sample

short_3 :: forall event. Cycle (Maybe (Note event))
short_3 = cycleFromSample S.short_3__Sample

short_4 :: forall event. Cycle (Maybe (Note event))
short_4 = cycleFromSample S.short_4__Sample

incoming :: forall event. Cycle (Maybe (Note event))
incoming = cycleFromSample S.incoming_0__Sample

incoming_0 :: forall event. Cycle (Maybe (Note event))
incoming_0 = cycleFromSample S.incoming_0__Sample

incoming_1 :: forall event. Cycle (Maybe (Note event))
incoming_1 = cycleFromSample S.incoming_1__Sample

incoming_2 :: forall event. Cycle (Maybe (Note event))
incoming_2 = cycleFromSample S.incoming_2__Sample

incoming_3 :: forall event. Cycle (Maybe (Note event))
incoming_3 = cycleFromSample S.incoming_3__Sample

incoming_4 :: forall event. Cycle (Maybe (Note event))
incoming_4 = cycleFromSample S.incoming_4__Sample

incoming_5 :: forall event. Cycle (Maybe (Note event))
incoming_5 = cycleFromSample S.incoming_5__Sample

incoming_6 :: forall event. Cycle (Maybe (Note event))
incoming_6 = cycleFromSample S.incoming_6__Sample

incoming_7 :: forall event. Cycle (Maybe (Note event))
incoming_7 = cycleFromSample S.incoming_7__Sample

flick :: forall event. Cycle (Maybe (Note event))
flick = cycleFromSample S.flick_0__Sample

flick_0 :: forall event. Cycle (Maybe (Note event))
flick_0 = cycleFromSample S.flick_0__Sample

flick_1 :: forall event. Cycle (Maybe (Note event))
flick_1 = cycleFromSample S.flick_1__Sample

flick_2 :: forall event. Cycle (Maybe (Note event))
flick_2 = cycleFromSample S.flick_2__Sample

flick_3 :: forall event. Cycle (Maybe (Note event))
flick_3 = cycleFromSample S.flick_3__Sample

flick_4 :: forall event. Cycle (Maybe (Note event))
flick_4 = cycleFromSample S.flick_4__Sample

flick_5 :: forall event. Cycle (Maybe (Note event))
flick_5 = cycleFromSample S.flick_5__Sample

flick_6 :: forall event. Cycle (Maybe (Note event))
flick_6 = cycleFromSample S.flick_6__Sample

flick_7 :: forall event. Cycle (Maybe (Note event))
flick_7 = cycleFromSample S.flick_7__Sample

flick_8 :: forall event. Cycle (Maybe (Note event))
flick_8 = cycleFromSample S.flick_8__Sample

flick_9 :: forall event. Cycle (Maybe (Note event))
flick_9 = cycleFromSample S.flick_9__Sample

flick_10 :: forall event. Cycle (Maybe (Note event))
flick_10 = cycleFromSample S.flick_10__Sample

flick_11 :: forall event. Cycle (Maybe (Note event))
flick_11 = cycleFromSample S.flick_11__Sample

flick_12 :: forall event. Cycle (Maybe (Note event))
flick_12 = cycleFromSample S.flick_12__Sample

flick_13 :: forall event. Cycle (Maybe (Note event))
flick_13 = cycleFromSample S.flick_13__Sample

flick_14 :: forall event. Cycle (Maybe (Note event))
flick_14 = cycleFromSample S.flick_14__Sample

flick_15 :: forall event. Cycle (Maybe (Note event))
flick_15 = cycleFromSample S.flick_15__Sample

flick_16 :: forall event. Cycle (Maybe (Note event))
flick_16 = cycleFromSample S.flick_16__Sample

reverbkick :: forall event. Cycle (Maybe (Note event))
reverbkick = cycleFromSample S.reverbkick_0__Sample

reverbkick_0 :: forall event. Cycle (Maybe (Note event))
reverbkick_0 = cycleFromSample S.reverbkick_0__Sample

bass2 :: forall event. Cycle (Maybe (Note event))
bass2 = cycleFromSample S.bass2_0__Sample

bass2_0 :: forall event. Cycle (Maybe (Note event))
bass2_0 = cycleFromSample S.bass2_0__Sample

bass2_1 :: forall event. Cycle (Maybe (Note event))
bass2_1 = cycleFromSample S.bass2_1__Sample

bass2_2 :: forall event. Cycle (Maybe (Note event))
bass2_2 = cycleFromSample S.bass2_2__Sample

bass2_3 :: forall event. Cycle (Maybe (Note event))
bass2_3 = cycleFromSample S.bass2_3__Sample

bass2_4 :: forall event. Cycle (Maybe (Note event))
bass2_4 = cycleFromSample S.bass2_4__Sample

baa :: forall event. Cycle (Maybe (Note event))
baa = cycleFromSample S.baa_0__Sample

baa_0 :: forall event. Cycle (Maybe (Note event))
baa_0 = cycleFromSample S.baa_0__Sample

baa_1 :: forall event. Cycle (Maybe (Note event))
baa_1 = cycleFromSample S.baa_1__Sample

baa_2 :: forall event. Cycle (Maybe (Note event))
baa_2 = cycleFromSample S.baa_2__Sample

baa_3 :: forall event. Cycle (Maybe (Note event))
baa_3 = cycleFromSample S.baa_3__Sample

baa_4 :: forall event. Cycle (Maybe (Note event))
baa_4 = cycleFromSample S.baa_4__Sample

baa_5 :: forall event. Cycle (Maybe (Note event))
baa_5 = cycleFromSample S.baa_5__Sample

baa_6 :: forall event. Cycle (Maybe (Note event))
baa_6 = cycleFromSample S.baa_6__Sample

fm :: forall event. Cycle (Maybe (Note event))
fm = cycleFromSample S.fm_0__Sample

fm_0 :: forall event. Cycle (Maybe (Note event))
fm_0 = cycleFromSample S.fm_0__Sample

fm_1 :: forall event. Cycle (Maybe (Note event))
fm_1 = cycleFromSample S.fm_1__Sample

fm_2 :: forall event. Cycle (Maybe (Note event))
fm_2 = cycleFromSample S.fm_2__Sample

fm_3 :: forall event. Cycle (Maybe (Note event))
fm_3 = cycleFromSample S.fm_3__Sample

fm_4 :: forall event. Cycle (Maybe (Note event))
fm_4 = cycleFromSample S.fm_4__Sample

fm_5 :: forall event. Cycle (Maybe (Note event))
fm_5 = cycleFromSample S.fm_5__Sample

fm_6 :: forall event. Cycle (Maybe (Note event))
fm_6 = cycleFromSample S.fm_6__Sample

fm_7 :: forall event. Cycle (Maybe (Note event))
fm_7 = cycleFromSample S.fm_7__Sample

fm_8 :: forall event. Cycle (Maybe (Note event))
fm_8 = cycleFromSample S.fm_8__Sample

fm_9 :: forall event. Cycle (Maybe (Note event))
fm_9 = cycleFromSample S.fm_9__Sample

fm_10 :: forall event. Cycle (Maybe (Note event))
fm_10 = cycleFromSample S.fm_10__Sample

fm_11 :: forall event. Cycle (Maybe (Note event))
fm_11 = cycleFromSample S.fm_11__Sample

fm_12 :: forall event. Cycle (Maybe (Note event))
fm_12 = cycleFromSample S.fm_12__Sample

fm_13 :: forall event. Cycle (Maybe (Note event))
fm_13 = cycleFromSample S.fm_13__Sample

fm_14 :: forall event. Cycle (Maybe (Note event))
fm_14 = cycleFromSample S.fm_14__Sample

fm_15 :: forall event. Cycle (Maybe (Note event))
fm_15 = cycleFromSample S.fm_15__Sample

fm_16 :: forall event. Cycle (Maybe (Note event))
fm_16 = cycleFromSample S.fm_16__Sample

click :: forall event. Cycle (Maybe (Note event))
click = cycleFromSample S.click_0__Sample

click_0 :: forall event. Cycle (Maybe (Note event))
click_0 = cycleFromSample S.click_0__Sample

click_1 :: forall event. Cycle (Maybe (Note event))
click_1 = cycleFromSample S.click_1__Sample

click_2 :: forall event. Cycle (Maybe (Note event))
click_2 = cycleFromSample S.click_2__Sample

click_3 :: forall event. Cycle (Maybe (Note event))
click_3 = cycleFromSample S.click_3__Sample

control :: forall event. Cycle (Maybe (Note event))
control = cycleFromSample S.control_0__Sample

control_0 :: forall event. Cycle (Maybe (Note event))
control_0 = cycleFromSample S.control_0__Sample

control_1 :: forall event. Cycle (Maybe (Note event))
control_1 = cycleFromSample S.control_1__Sample

peri :: forall event. Cycle (Maybe (Note event))
peri = cycleFromSample S.peri_0__Sample

peri_0 :: forall event. Cycle (Maybe (Note event))
peri_0 = cycleFromSample S.peri_0__Sample

peri_1 :: forall event. Cycle (Maybe (Note event))
peri_1 = cycleFromSample S.peri_1__Sample

peri_2 :: forall event. Cycle (Maybe (Note event))
peri_2 = cycleFromSample S.peri_2__Sample

peri_3 :: forall event. Cycle (Maybe (Note event))
peri_3 = cycleFromSample S.peri_3__Sample

peri_4 :: forall event. Cycle (Maybe (Note event))
peri_4 = cycleFromSample S.peri_4__Sample

peri_5 :: forall event. Cycle (Maybe (Note event))
peri_5 = cycleFromSample S.peri_5__Sample

peri_6 :: forall event. Cycle (Maybe (Note event))
peri_6 = cycleFromSample S.peri_6__Sample

peri_7 :: forall event. Cycle (Maybe (Note event))
peri_7 = cycleFromSample S.peri_7__Sample

peri_8 :: forall event. Cycle (Maybe (Note event))
peri_8 = cycleFromSample S.peri_8__Sample

peri_9 :: forall event. Cycle (Maybe (Note event))
peri_9 = cycleFromSample S.peri_9__Sample

peri_10 :: forall event. Cycle (Maybe (Note event))
peri_10 = cycleFromSample S.peri_10__Sample

peri_11 :: forall event. Cycle (Maybe (Note event))
peri_11 = cycleFromSample S.peri_11__Sample

peri_12 :: forall event. Cycle (Maybe (Note event))
peri_12 = cycleFromSample S.peri_12__Sample

peri_13 :: forall event. Cycle (Maybe (Note event))
peri_13 = cycleFromSample S.peri_13__Sample

peri_14 :: forall event. Cycle (Maybe (Note event))
peri_14 = cycleFromSample S.peri_14__Sample

procshort :: forall event. Cycle (Maybe (Note event))
procshort = cycleFromSample S.procshort_0__Sample

procshort_0 :: forall event. Cycle (Maybe (Note event))
procshort_0 = cycleFromSample S.procshort_0__Sample

procshort_1 :: forall event. Cycle (Maybe (Note event))
procshort_1 = cycleFromSample S.procshort_1__Sample

procshort_2 :: forall event. Cycle (Maybe (Note event))
procshort_2 = cycleFromSample S.procshort_2__Sample

procshort_3 :: forall event. Cycle (Maybe (Note event))
procshort_3 = cycleFromSample S.procshort_3__Sample

procshort_4 :: forall event. Cycle (Maybe (Note event))
procshort_4 = cycleFromSample S.procshort_4__Sample

procshort_5 :: forall event. Cycle (Maybe (Note event))
procshort_5 = cycleFromSample S.procshort_5__Sample

procshort_6 :: forall event. Cycle (Maybe (Note event))
procshort_6 = cycleFromSample S.procshort_6__Sample

procshort_7 :: forall event. Cycle (Maybe (Note event))
procshort_7 = cycleFromSample S.procshort_7__Sample

hand :: forall event. Cycle (Maybe (Note event))
hand = cycleFromSample S.hand_0__Sample

hand_0 :: forall event. Cycle (Maybe (Note event))
hand_0 = cycleFromSample S.hand_0__Sample

hand_1 :: forall event. Cycle (Maybe (Note event))
hand_1 = cycleFromSample S.hand_1__Sample

hand_2 :: forall event. Cycle (Maybe (Note event))
hand_2 = cycleFromSample S.hand_2__Sample

hand_3 :: forall event. Cycle (Maybe (Note event))
hand_3 = cycleFromSample S.hand_3__Sample

hand_4 :: forall event. Cycle (Maybe (Note event))
hand_4 = cycleFromSample S.hand_4__Sample

hand_5 :: forall event. Cycle (Maybe (Note event))
hand_5 = cycleFromSample S.hand_5__Sample

hand_6 :: forall event. Cycle (Maybe (Note event))
hand_6 = cycleFromSample S.hand_6__Sample

hand_7 :: forall event. Cycle (Maybe (Note event))
hand_7 = cycleFromSample S.hand_7__Sample

hand_8 :: forall event. Cycle (Maybe (Note event))
hand_8 = cycleFromSample S.hand_8__Sample

hand_9 :: forall event. Cycle (Maybe (Note event))
hand_9 = cycleFromSample S.hand_9__Sample

hand_10 :: forall event. Cycle (Maybe (Note event))
hand_10 = cycleFromSample S.hand_10__Sample

hand_11 :: forall event. Cycle (Maybe (Note event))
hand_11 = cycleFromSample S.hand_11__Sample

hand_12 :: forall event. Cycle (Maybe (Note event))
hand_12 = cycleFromSample S.hand_12__Sample

hand_13 :: forall event. Cycle (Maybe (Note event))
hand_13 = cycleFromSample S.hand_13__Sample

hand_14 :: forall event. Cycle (Maybe (Note event))
hand_14 = cycleFromSample S.hand_14__Sample

hand_15 :: forall event. Cycle (Maybe (Note event))
hand_15 = cycleFromSample S.hand_15__Sample

hand_16 :: forall event. Cycle (Maybe (Note event))
hand_16 = cycleFromSample S.hand_16__Sample

future :: forall event. Cycle (Maybe (Note event))
future = cycleFromSample S.future_0__Sample

future_0 :: forall event. Cycle (Maybe (Note event))
future_0 = cycleFromSample S.future_0__Sample

future_1 :: forall event. Cycle (Maybe (Note event))
future_1 = cycleFromSample S.future_1__Sample

future_2 :: forall event. Cycle (Maybe (Note event))
future_2 = cycleFromSample S.future_2__Sample

future_3 :: forall event. Cycle (Maybe (Note event))
future_3 = cycleFromSample S.future_3__Sample

future_4 :: forall event. Cycle (Maybe (Note event))
future_4 = cycleFromSample S.future_4__Sample

future_5 :: forall event. Cycle (Maybe (Note event))
future_5 = cycleFromSample S.future_5__Sample

future_6 :: forall event. Cycle (Maybe (Note event))
future_6 = cycleFromSample S.future_6__Sample

future_7 :: forall event. Cycle (Maybe (Note event))
future_7 = cycleFromSample S.future_7__Sample

future_8 :: forall event. Cycle (Maybe (Note event))
future_8 = cycleFromSample S.future_8__Sample

future_9 :: forall event. Cycle (Maybe (Note event))
future_9 = cycleFromSample S.future_9__Sample

future_10 :: forall event. Cycle (Maybe (Note event))
future_10 = cycleFromSample S.future_10__Sample

future_11 :: forall event. Cycle (Maybe (Note event))
future_11 = cycleFromSample S.future_11__Sample

future_12 :: forall event. Cycle (Maybe (Note event))
future_12 = cycleFromSample S.future_12__Sample

future_13 :: forall event. Cycle (Maybe (Note event))
future_13 = cycleFromSample S.future_13__Sample

future_14 :: forall event. Cycle (Maybe (Note event))
future_14 = cycleFromSample S.future_14__Sample

future_15 :: forall event. Cycle (Maybe (Note event))
future_15 = cycleFromSample S.future_15__Sample

future_16 :: forall event. Cycle (Maybe (Note event))
future_16 = cycleFromSample S.future_16__Sample

hh :: forall event. Cycle (Maybe (Note event))
hh = cycleFromSample S.hh_0__Sample

hh_0 :: forall event. Cycle (Maybe (Note event))
hh_0 = cycleFromSample S.hh_0__Sample

hh_1 :: forall event. Cycle (Maybe (Note event))
hh_1 = cycleFromSample S.hh_1__Sample

hh_2 :: forall event. Cycle (Maybe (Note event))
hh_2 = cycleFromSample S.hh_2__Sample

hh_3 :: forall event. Cycle (Maybe (Note event))
hh_3 = cycleFromSample S.hh_3__Sample

hh_4 :: forall event. Cycle (Maybe (Note event))
hh_4 = cycleFromSample S.hh_4__Sample

hh_5 :: forall event. Cycle (Maybe (Note event))
hh_5 = cycleFromSample S.hh_5__Sample

hh_6 :: forall event. Cycle (Maybe (Note event))
hh_6 = cycleFromSample S.hh_6__Sample

hh_7 :: forall event. Cycle (Maybe (Note event))
hh_7 = cycleFromSample S.hh_7__Sample

hh_8 :: forall event. Cycle (Maybe (Note event))
hh_8 = cycleFromSample S.hh_8__Sample

hh_9 :: forall event. Cycle (Maybe (Note event))
hh_9 = cycleFromSample S.hh_9__Sample

hh_10 :: forall event. Cycle (Maybe (Note event))
hh_10 = cycleFromSample S.hh_10__Sample

hh_11 :: forall event. Cycle (Maybe (Note event))
hh_11 = cycleFromSample S.hh_11__Sample

hh_12 :: forall event. Cycle (Maybe (Note event))
hh_12 = cycleFromSample S.hh_12__Sample

x_808ht :: forall event. Cycle (Maybe (Note event))
x_808ht = cycleFromSample S.x_808ht_0__Sample

x_808ht_0 :: forall event. Cycle (Maybe (Note event))
x_808ht_0 = cycleFromSample S.x_808ht_0__Sample

x_808ht_1 :: forall event. Cycle (Maybe (Note event))
x_808ht_1 = cycleFromSample S.x_808ht_1__Sample

x_808ht_2 :: forall event. Cycle (Maybe (Note event))
x_808ht_2 = cycleFromSample S.x_808ht_2__Sample

x_808ht_3 :: forall event. Cycle (Maybe (Note event))
x_808ht_3 = cycleFromSample S.x_808ht_3__Sample

x_808ht_4 :: forall event. Cycle (Maybe (Note event))
x_808ht_4 = cycleFromSample S.x_808ht_4__Sample

db :: forall event. Cycle (Maybe (Note event))
db = cycleFromSample S.db_0__Sample

db_0 :: forall event. Cycle (Maybe (Note event))
db_0 = cycleFromSample S.db_0__Sample

db_1 :: forall event. Cycle (Maybe (Note event))
db_1 = cycleFromSample S.db_1__Sample

db_2 :: forall event. Cycle (Maybe (Note event))
db_2 = cycleFromSample S.db_2__Sample

db_3 :: forall event. Cycle (Maybe (Note event))
db_3 = cycleFromSample S.db_3__Sample

db_4 :: forall event. Cycle (Maybe (Note event))
db_4 = cycleFromSample S.db_4__Sample

db_5 :: forall event. Cycle (Maybe (Note event))
db_5 = cycleFromSample S.db_5__Sample

db_6 :: forall event. Cycle (Maybe (Note event))
db_6 = cycleFromSample S.db_6__Sample

db_7 :: forall event. Cycle (Maybe (Note event))
db_7 = cycleFromSample S.db_7__Sample

db_8 :: forall event. Cycle (Maybe (Note event))
db_8 = cycleFromSample S.db_8__Sample

db_9 :: forall event. Cycle (Maybe (Note event))
db_9 = cycleFromSample S.db_9__Sample

db_10 :: forall event. Cycle (Maybe (Note event))
db_10 = cycleFromSample S.db_10__Sample

db_11 :: forall event. Cycle (Maybe (Note event))
db_11 = cycleFromSample S.db_11__Sample

db_12 :: forall event. Cycle (Maybe (Note event))
db_12 = cycleFromSample S.db_12__Sample

--- drones

spacewind :: forall event. Cycle (Maybe (Note event))
spacewind = cycleFromSample S.spacewind_0__Sample

spacewind_0 :: forall event. Cycle (Maybe (Note event))
spacewind_0 = cycleFromSample S.spacewind_0__Sample

ambienta :: forall event. Cycle (Maybe (Note event))
ambienta = cycleFromSample S.ambienta_0__Sample

ambienta_0 :: forall event. Cycle (Maybe (Note event))
ambienta_0 = cycleFromSample S.ambienta_0__Sample

lowdark :: forall event. Cycle (Maybe (Note event))
lowdark = cycleFromSample S.lowdark_0__Sample

lowdark_0 :: forall event. Cycle (Maybe (Note event))
lowdark_0 = cycleFromSample S.lowdark_0__Sample

harmonium :: forall event. Cycle (Maybe (Note event))
harmonium = cycleFromSample S.harmonium_0__Sample

harmonium_0 :: forall event. Cycle (Maybe (Note event))
harmonium_0 = cycleFromSample S.harmonium_0__Sample

hollowair :: forall event. Cycle (Maybe (Note event))
hollowair = cycleFromSample S.hollowair_0__Sample

hollowair_0 :: forall event. Cycle (Maybe (Note event))
hollowair_0 = cycleFromSample S.hollowair_0__Sample

digeridoo :: forall event. Cycle (Maybe (Note event))
digeridoo = cycleFromSample S.digeridoo_0__Sample

digeridoo_0 :: forall event. Cycle (Maybe (Note event))
digeridoo_0 = cycleFromSample S.digeridoo_0__Sample

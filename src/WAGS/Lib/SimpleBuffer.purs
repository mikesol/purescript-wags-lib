module WAGS.Lib.SimpleBuffer where

import Prelude

import Control.Comonad (extract)
import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', lens, over)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Semigroup.First (First(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Pos)
import Data.Unfoldable as UF
import WAGS.Graph.Parameter (AudioParameter_(..))
import WAGS.Lib.BufferPool (ABufferPool, CfBufferPool, BuffyVec)
import WAGS.Lib.Latch (ALatchAP, CfLatchAP, LatchAP)
import WAGS.Lib.Piecewise (makeLoopingTerracedR)
import WAGS.Run (SceneI(..))

type SimpleBuffer nbuf
  =
  { latch :: ALatchAP (First (Maybe Int))
  , buffers :: ABufferPool nbuf Unit
  }

type SimpleBufferCf nbuf
  =
  { latch :: CfLatchAP (First (Maybe Int))
  , buffers :: CfBufferPool nbuf Unit
  }

type SimpleBufferHead nbuf
  = { latch :: LatchAP (First (Maybe Int)), buffers :: BuffyVec nbuf Unit }

neaHead :: forall n. Lens' (NEA.NonEmptyArray n) n
neaHead = lens NEA.head (\a b -> let (_ :| r) = NEA.toNonEmpty a in NEA.fromNonEmpty (b :| r))

nea2nel :: NEA.NonEmptyArray ~> NEL.NonEmptyList
nea2nel x = let (a :| b) = NEA.toNonEmpty x in NEL.NonEmptyList (a :| L.fromFoldable b)

actualizeSimpleBuffer
  :: forall trigger world analyserCallbacks nbuf
   . Pos nbuf
  => NEA.NonEmptyArray Number
  -> Number
  -> SceneI trigger world analyserCallbacks
  -> SimpleBuffer nbuf
  -> SimpleBufferCf nbuf
actualizeSimpleBuffer nea' end'' = go
  where
  nea = if NEA.length nea' == 1 then nea' <> map (add end'') nea' else nea'

  end' = if NEA.length nea' == 1 then 2.0 * end'' else end''

  end = if NEA.last nea >= end' then NEA.last nea + 0.0001 else end'

  step0 = over neaHead (\a -> if a < 0.0 then 0.0 else a) nea

  step1 = mapWithIndex (\i v -> v /\ (First $ Just i)) step0

  step2 = if (fst $ NEA.head step1) > 0.0 then NEA.cons (0.0 /\ (First $ Nothing)) step1 else step1

  step3 = NEA.snoc step2 (end /\ (snd $ NEA.head step2))

  (NEL.NonEmptyList step4) = nea2nel step3

  loopingTerraced = makeLoopingTerracedR step4
  -- (SceneI { time, headroomInSeconds: headroom }) offsets = r { time, headroom, offsets }

  go (SceneI e'@{ time, headroomInSeconds: headroom }) { latch: latch', buffers } =
    { latch
    , buffers:
        buffers
          { time
          , headroom
          , offsets: UF.fromMaybe do
              AudioParameter { param, timeOffset } <- extract latch
              (First param') <- param
              _ <- param'
              pure { offset: timeOffset, rest: unit }
          }
    }
    where
    fromPW = loopingTerraced { time: e'.time, headroom: e'.headroomInSeconds }

    latch = latch' fromPW

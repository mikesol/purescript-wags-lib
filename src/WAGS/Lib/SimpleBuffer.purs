module WAGS.Lib.SimpleBuffer where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Array.NonEmpty (NonEmptyArray)
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
import Data.Unfoldable as UF
import WAGS.Graph.Parameter (AudioParameter_(..))
import WAGS.Lib.BufferPool (ABufferPool, BuffyVec, CfBufferPool, TimeOffsets)
import WAGS.Lib.Latch (ALatchAP, CfLatchAP, LatchAP)
import WAGS.Lib.Piecewise (makeLoopingTerracedR)
import WAGS.Run (SceneI(..))

type SimpleBuffer' nbuf r
  =
  ( latch :: ALatchAP (First (Maybe Int))
  , buffers :: ABufferPool nbuf
  | r
  )

type SimpleBufferCf' nbuf r
  =
  ( latch :: CfLatchAP (First (Maybe Int))
  , buffers :: CfBufferPool nbuf
  | r
  )

type SimpleBufferHead' nbuf r
  = (latch :: LatchAP (First (Maybe Int)), buffers :: BuffyVec nbuf | r)

type SimpleBuffer nbuf
  = { | SimpleBuffer' nbuf () }

type SimpleBufferCf nbuf
  = { | SimpleBufferCf' nbuf () }

type SimpleBufferHead nbuf
  = { | SimpleBufferHead' nbuf () }

neaHead :: forall n. Lens' (NEA.NonEmptyArray n) n
neaHead = lens NEA.head (\a b -> let (_ :| r) = NEA.toNonEmpty a in NEA.fromNonEmpty (b :| r))

nea2nel :: NEA.NonEmptyArray ~> NEL.NonEmptyList
nea2nel x = let (a :| b) = NEA.toNonEmpty x in NEL.NonEmptyList (a :| L.fromFoldable b)

actualizeSimpleBuffer
  :: forall trigger world analyserCbs r w latched cofree
   . Comonad w
  => NonEmptyArray Number
  -> Number
  -> SceneI trigger world analyserCbs
  -> { buffers :: TimeOffsets -> cofree
     , latch :: AudioParameter_ (First (Maybe Int)) -> w (Maybe (AudioParameter_ (First (Maybe latched))))
     | r
     }
  -> { buffers :: cofree
     , latch :: w (Maybe (AudioParameter_ (First (Maybe latched))))
     }
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

  go (SceneI e'@{ time }) { latch: latch', buffers } =
    { latch
    , buffers:
        buffers
          { time
          , offsets: UF.fromMaybe do
              AudioParameter { param, timeOffset } <- extract latch
              (First param') <- param
              _ <- param'
              pure { offset: timeOffset }
          }
    }
    where
    fromPW = loopingTerraced { time: e'.time, headroomInSeconds: e'.headroomInSeconds }

    latch = latch' fromPW

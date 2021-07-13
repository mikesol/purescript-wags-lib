-- | Piecewise functions
module WAGS.Lib.Piecewise where

import Prelude
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..), sortBy)
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set (Set, toMap)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Math ((%))
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter_(..), AudioParameter)
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroom :: Number }

type APFofT
  = TimeHeadroom -> AudioParameter

newtype PWChunk
  = PWChunk { left :: Number, right :: Number, apfot :: APFofT }

minValue = -5e300 :: Number

maxValue = 5e300 :: Number

instance eqPWChunk :: Eq PWChunk where
  eq (PWChunk { left: left0 }) (PWChunk { left: left1 }) = eq left0 left1

instance ordPWChunk :: Ord PWChunk where
  compare (PWChunk { left: left0 }) (PWChunk { left: left1 }) = compare left0 left1

lookupLE :: forall a. Ord a => a -> Set a -> Maybe a
lookupLE a s = map _.key (Map.lookupLE a (toMap s))

type TLR_AP
  = { time :: Number, left :: Number /\ Number, right :: Number /\ Number } -> AudioParameter

makeChunks' :: TLR_AP -> TLR_AP -> NonEmpty List (Number /\ Number) -> NonEmpty List PWChunk
makeChunks' spillover inChunk l' = map PWChunk (go sorted)
  where
  (NonEmptyList sorted) = sortBy (\a b -> compare (fst a) (fst b)) (NonEmptyList l')

  go (a /\ b :| Nil) = { left: minValue, right: maxValue, apfot: const (pure b) } :| Nil

  go l@(hol :| tol) =
    { left: minValue, right: (fst $ NEL.head (wrap l)), apfot: const (pure (snd $ NEL.head (wrap l))) }
      :| ( ( foldl
                ( \{ acc, cur } a ->
                    { cur: a
                    , acc:
                        acc
                          <> pure
                              { left: fst cur
                              , right: fst a
                              , apfot:
                                  \{ time, headroom } ->
                                    let
                                      lookahead = time + headroom
                                    in
                                      ( if lookahead >= fst a then
                                          spillover
                                        else
                                          inChunk
                                      )
                                        { time, left: cur, right: a }
                              }
                    }
                )
                { cur: NEL.head (wrap l), acc: Nil }
                tol
            )
            .acc
            <> pure { right: maxValue, left: (fst $ NEL.last (wrap l)), apfot: const (pure (snd $ NEL.last (wrap l))) }
        )

type MakeChunks
  = NonEmpty List (Number /\ Number) -> NonEmpty List PWChunk

makeChunks :: MakeChunks
makeChunks =
  makeChunks'
    ( \{ time, right } ->
        AudioParameter
          { param: Just (snd right)
          , timeOffset: (fst right) - time
          , transition: LinearRamp
          }
    ) \{ time, left, right } ->
    AudioParameter
      { param: Just (calcSlope (fst left) (snd left) (fst right) (snd right) time)
      , timeOffset: 0.0
      , transition: LinearRamp
      }

makeChunksL :: MakeChunks
makeChunksL =
  makeChunks'
    ( \{ time, left, right } ->
        AudioParameter
          { param: Just (snd left)
          , timeOffset: (fst right) - time
          , transition: LinearRamp
          }
    ) \{ time, left, right } ->
    AudioParameter
      { param: Just (snd left)
      , timeOffset: 0.0
      , transition: LinearRamp
      }

makeChunksR :: MakeChunks
makeChunksR =
  makeChunks'
    ( \{ time, right } ->
        AudioParameter
          { param: Just (snd right)
          , timeOffset: (fst right) - time
          , transition: LinearRamp
          }
    ) \{ time, left, right } ->
    AudioParameter
      { param: Just (snd left)
      , timeOffset: 0.0
      , transition: LinearRamp
      }

type MakePiecewise
  = NonEmpty List (Number /\ Number) -> APFofT

makePiecewise' :: MakeChunks -> MakePiecewise
makePiecewise' mkc l = go
  where
  chunks = mkc l

  asSet = Set.fromFoldable chunks

  apfot' = const (pure 1.0)

  go th@{ time, headroom } = case lookupLE (PWChunk { left: time, right: 0.0, apfot: apfot' }) asSet of
    Nothing -> pure 1.0
    Just (PWChunk { apfot }) -> apfot th

makeLoopingPiecewise' :: MakePiecewise -> MakePiecewise
makeLoopingPiecewise' mpw l { time, headroom } = mpw l { time: time % (foldl max 0.0 (map fst l)), headroom }

makePiecewise :: MakePiecewise
makePiecewise = makePiecewise' makeChunks

makeLoopingPiecewise :: MakePiecewise
makeLoopingPiecewise = makeLoopingPiecewise' makePiecewise

makeTerracedL :: MakePiecewise
makeTerracedL = makePiecewise' makeChunksL

makeLoopingTerracedL :: MakePiecewise
makeLoopingTerracedL = makeLoopingPiecewise' makeTerracedL

makeTerracedR :: MakePiecewise
makeTerracedR = makePiecewise' makeChunksR

makeLoopingTerracedR :: MakePiecewise
makeLoopingTerracedR = makeLoopingPiecewise' makeTerracedR
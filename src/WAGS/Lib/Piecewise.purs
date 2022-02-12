-- | Piecewise functions
module WAGS.Lib.Piecewise where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
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
import Data.Variant.Maybe (just)
import Math ((%))
import WAGS.Graph.Parameter (AudioParameter_(..), _linearRamp)
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroomInSeconds :: Number }

type APFofT v
  = TimeHeadroom -> AudioParameter_ v

data PWChunk v
  = PWChunk { left :: Number, right :: Number, apfot :: (APFofT v) }
  | Beacon Number

getLeft :: forall v. PWChunk v -> Number
getLeft = case _ of
  PWChunk { left } -> left
  Beacon left -> left

newtype PWRealized v
  = PWRealized { left :: Number, right :: Number, apfot :: (APFofT v) }

minValue = -5e300 :: Number

maxValue = 5e300 :: Number

instance eqPWChunk :: Eq (PWChunk v) where
  eq a b = eq (getLeft a) (getLeft b)

instance ordPWChunk :: Ord (PWChunk v) where
  compare a b = compare (getLeft a) (getLeft b)

lookupLE :: forall v. PWChunk v -> Set (PWChunk v) -> Maybe (PWRealized v)
lookupLE a s = (map _.key (Map.lookupLE a (toMap s))) >>= case _ of
  PWChunk x -> Just (PWRealized x)
  _ -> Nothing

type TLR_AP v
  = { time :: Number, left :: Number /\ v, right :: Number /\ v } -> AudioParameter_ v

makeChunks' :: forall v. TLR_AP v -> TLR_AP v -> MakeChunks v
makeChunks' spillover inChunk l' = (snd shead) /\ map PWChunk (go sorted)
  where
  (NonEmptyList sorted@(shead :| _)) = sortBy (\a b -> compare (fst a) (fst b)) (NonEmptyList l')

  go (_ /\ b :| Nil) = { left: minValue, right: maxValue, apfot: const (pure b) } :| Nil

  go l@(_ :| tol) =
    { left: minValue, right: (fst $ NEL.head (wrap l)), apfot: const (pure (snd $ NEL.head (wrap l))) }
      :|
        ( ( foldl
              ( \{ acc, cur } a ->
                  { cur: a
                  , acc:
                      acc
                        <> pure
                          { left: fst cur
                          , right: fst a
                          , apfot:
                              \{ time, headroomInSeconds } ->
                                let
                                  lookahead = time + headroomInSeconds
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
          ).acc
            <> pure { right: maxValue, left: (fst $ NEL.last (wrap l)), apfot: const (pure (snd $ NEL.last (wrap l))) }
        )

type MakeChunks v
  = NonEmpty List (Number /\ v) -> v /\ NonEmpty List (PWChunk v)

makeChunks :: MakeChunks Number
makeChunks =
  makeChunks'
    ( \{ time, right } ->
        AudioParameter
          { param: just (snd right)
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ time, left, right } ->
      AudioParameter
        { param: just (calcSlope (fst left) (snd left) (fst right) (snd right) time)
        , timeOffset: 0.0
        , transition: _linearRamp
        }

makeChunksL :: forall v. MakeChunks v
makeChunksL =
  makeChunks'
    ( \{ time, left, right } ->
        AudioParameter
          { param: just (snd left)
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ left } ->
      AudioParameter
        { param: just (snd left)
        , timeOffset: 0.0
        , transition: _linearRamp
        }

makeChunksR :: forall v. MakeChunks v
makeChunksR =
  makeChunks'
    ( \{ time, right } ->
        AudioParameter
          { param: just (snd right)
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ left } ->
      AudioParameter
        { param: just (snd left)
        , timeOffset: 0.0
        , transition: _linearRamp
        }

type MakePiecewise v
  = NonEmpty List (Number /\ v) -> (APFofT v)

type MakePiecewiseA v
  = Array (Number /\ v) -> (APFofT v)

makePiecewise' :: forall v. MakeChunks v -> MakePiecewise v
makePiecewise' mkc l = go
  where
  defaultV /\ chunks = mkc l

  asSet = Set.fromFoldable chunks

  go th@{ time } = case lookupLE (Beacon time) asSet of
    Nothing -> pure defaultV
    Just (PWRealized { apfot }) -> apfot th

makeLoopingPiecewise' :: MakePiecewise ~> MakePiecewise
makeLoopingPiecewise' mpw l { time, headroomInSeconds } = mpw l { time: time % (foldl max 0.0 (map fst l)), headroomInSeconds }

makePiecewise :: MakePiecewise Number
makePiecewise = makePiecewise' makeChunks

makePiecewiseA :: MakePiecewiseA Number
makePiecewiseA = NEA.fromArray >>> map NEA.toNonEmpty >>> case _ of
  Just (aa :| bb) -> makePiecewise (aa :| List.fromFoldable bb)
  Nothing -> makePiecewise (0.0 /\ 0.0 :| Nil)

makeLoopingPiecewise :: MakePiecewise Number
makeLoopingPiecewise = makeLoopingPiecewise' makePiecewise

makeTerracedL :: forall v. MakePiecewise v
makeTerracedL = makePiecewise' makeChunksL

makeLoopingTerracedL :: forall v. MakePiecewise v
makeLoopingTerracedL = makeLoopingPiecewise' makeTerracedL

makeTerracedR :: forall v. MakePiecewise v
makeTerracedR = makePiecewise' makeChunksR

makeLoopingTerracedR :: forall v. MakePiecewise v
makeLoopingTerracedR = makeLoopingPiecewise' makeTerracedR

simplePiecewise :: Array (Number /\ Number) -> Number -> Number
simplePiecewise arr = f
  where
  asMap = Map.fromFoldable arr
  f n = val
    where
    lb = Map.lookupLE n asMap
    ub = Map.lookupGE n asMap
    val = case lb of
      Nothing -> case ub of
        Nothing -> 0.0
        Just x -> x.value
      Just llb -> case ub of
        Nothing -> llb.value
        Just uub -> calcSlope llb.key llb.value uub.key uub.value n

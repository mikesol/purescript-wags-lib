-- | Piecewise functions
module WAGS.Lib.Piecewise where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (foldl)
import Data.Lens (set)
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
import Math ((%))
import WAGS.Graph.Parameter (AudioSingleNumber(..), AudioSingleNumber, _linearRamp, lensParam, singleNumber)
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroomInSeconds :: Number }

type APFofT
  = TimeHeadroom -> AudioSingleNumber

data PWChunk
  = PWChunk { left :: Number, right :: Number, apfot :: APFofT }
  | Beacon Number

getLeft :: PWChunk -> Number
getLeft = case _ of
  PWChunk { left } -> left
  Beacon left -> left

newtype PWRealized
  = PWRealized { left :: Number, right :: Number, apfot :: APFofT }

minValue = -5e300 :: Number

maxValue = 5e300 :: Number

instance eqPWChunk :: Eq PWChunk where
  eq a b = eq (getLeft a) (getLeft b)

instance ordPWChunk :: Ord PWChunk where
  compare a b = compare (getLeft a) (getLeft b)

lookupLE :: PWChunk -> Set (PWChunk) -> Maybe (PWRealized)
lookupLE a s = (map _.key (Map.lookupLE a (toMap s))) >>= case _ of
  PWChunk x -> Just (PWRealized x)
  _ -> Nothing

type TLR_AP
  = { time :: Number, left :: Number /\ Number, right :: Number /\ Number } -> AudioSingleNumber

makeChunks' :: TLR_AP -> TLR_AP -> MakeChunks
makeChunks' spillover inChunk l' = (snd shead) /\ map PWChunk (go sorted)
  where
  (NonEmptyList sorted@(shead :| _)) = sortBy (\a b -> compare (fst a) (fst b)) (NonEmptyList l')

  go (_ /\ b :| Nil) = { left: minValue, right: maxValue, apfot: const (asSingleValue b) } :| Nil

  go l@(_ :| tol) =
    { left: minValue, right: (fst $ NEL.head (wrap l)), apfot: const (asSingleValue (snd $ NEL.head (wrap l))) }
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
            <> pure
              { right: maxValue
              , left: (fst $ NEL.last (wrap l))
              , apfot: const (asSingleValue (snd $ NEL.last (wrap l)))
              }
        )

type MakeChunks
  = NonEmpty List (Number /\ Number) -> Number /\ NonEmpty List PWChunk

makeChunks :: MakeChunks
makeChunks =
  makeChunks'
    ( \{ time, right } ->
         AudioSingleNumber
          { param: (snd right)
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ time, left, right } ->
       AudioSingleNumber
        { param: (calcSlope (fst left) (snd left) (fst right) (snd right) time)
        , timeOffset: 0.0
        , transition: _linearRamp
        }

makeChunksL :: MakeChunks
makeChunksL =
  makeChunks'
    ( \{ time, left, right } ->
         AudioSingleNumber
          { param: snd left
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ left } ->
       AudioSingleNumber
        { param: snd left
        , timeOffset: 0.0
        , transition: _linearRamp
        }

makeChunksR :: MakeChunks
makeChunksR =
  makeChunks'
    ( \{ time, right } ->
         AudioSingleNumber
          { param: snd right
          , timeOffset: (fst right) - time
          , transition: _linearRamp
          }
    )
    \{ left } ->
       AudioSingleNumber
        { param: snd left
        , timeOffset: 0.0
        , transition: _linearRamp
        }

type MakePiecewise
  = NonEmpty List (Number /\ Number) -> APFofT

type MakePiecewiseA
  = Array (Number /\ Number) -> APFofT

asSingleValue :: Number -> AudioSingleNumber
asSingleValue = flip (set lensParam) mempty

makePiecewise' :: MakeChunks -> MakePiecewise
makePiecewise' mkc l = go
  where
  defaultV /\ chunks = mkc l

  asSet = Set.fromFoldable chunks

  go th@{ time } = case lookupLE (Beacon time) asSet of
    Nothing -> asSingleValue defaultV
    Just (PWRealized { apfot }) -> apfot th

makeLoopingPiecewise' :: MakePiecewise -> MakePiecewise
makeLoopingPiecewise' mpw l { time, headroomInSeconds } = mpw l { time: time % (foldl max 0.0 (map fst l)), headroomInSeconds }

makePiecewise :: MakePiecewise
makePiecewise = makePiecewise' makeChunks

makePiecewiseA :: MakePiecewiseA
makePiecewiseA = NEA.fromArray >>> map NEA.toNonEmpty >>> case _ of
  Just (aa :| bb) -> makePiecewise (aa :| List.fromFoldable bb)
  Nothing -> makePiecewise (0.0 /\ 0.0 :| Nil)

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

-- | Piecewise functions
module WAGS.Lib.SFofT where

import Prelude
import Data.Foldable (foldl)
import Data.List (List(..), (:))
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
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter, AudioParameter_(..))
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroom :: Number }

type APFofT
  = TimeHeadroom -> AudioParameter

makeLoopingPiecewise :: NonEmpty List (Number /\ Number) -> APFofT
makeLoopingPiecewise l { time, headroom } = makePiecewise l { time: time % (foldl max 0.0 (map fst l)), headroom }

infixl 6 makeLoopingPiecewise as /@:<

newtype PWChunk
  = PWChunk { left :: Number, right :: Number, apfot :: APFofT }

foreign import minValue :: Number

foreign import maxValue :: Number

instance eqPWChunk :: Eq PWChunk where
  eq (PWChunk { left: left0 }) (PWChunk { left: left1 }) = eq left0 left1

instance ordPWChunk :: Ord PWChunk where
  compare (PWChunk { left: left0 }) (PWChunk { left: left1 }) = compare left0 left1

lookupLE :: forall a. Ord a => a -> Set a -> Maybe a
lookupLE a s = map _.key (Map.lookupLE a (toMap s))

makeChunks :: NonEmpty List (Number /\ Number) -> NonEmpty List PWChunk
makeChunks l' = map PWChunk (go sorted)
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
                                          AudioParameter
                                            { param: Just (snd a)
                                            , timeOffset: (fst a) - time
                                            , transition: LinearRamp
                                            }
                                        else
                                          AudioParameter { param: Just (calcSlope (fst cur) (snd cur) (fst a) (snd a) time), timeOffset: 0.0, transition: LinearRamp }
                                      )
                              }
                    }
                )
                { cur: NEL.head (wrap l), acc: Nil }
                (hol : tol)
            )
            .acc
            <> pure { right: maxValue, left: (fst $ NEL.last (wrap l)), apfot: const (pure (snd $ NEL.last (wrap l))) }
        )

makePiecewise :: NonEmpty List (Number /\ Number) -> APFofT
makePiecewise l = go
  where
  chunks = makeChunks l

  asSet = Set.fromFoldable chunks

  apfot' = const (pure 1.0)

  go th@{ time, headroom } = case lookupLE (PWChunk { left: time, right: 0.0, apfot: apfot' }) asSet of
    Nothing -> pure 1.0
    Just (PWChunk { apfot }) -> apfot th

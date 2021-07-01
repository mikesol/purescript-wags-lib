-- | SFofT is "streaming function of time"
-- | Writing functions of time can often lead to computationally inefficient outcomes when there
-- | are many branching conditions. For example, if there are 1000 branching conditions before
-- | you get to the timestamp you want, it will require 1000 boolean checks just to calculate the input.
-- | If the functions could be indexed by Ord, this could be reduced to O(log(n)) time, but in general
-- | functions are not indexable.
-- | One good strategy is to take advantage of the fact that time is always moving forward, often in small
-- | increments. This means that the next result will likely be slightly after the previous one.
-- | Knowing this, the number of branching conditions is quite limited, as they are restricted to a small
-- | number of timestamps, if any, ahead of the current one.
-- | This is achieved here with a non-halting Mealy machine implemented using Cofree Comonad, or a streaming
-- | function of time.

module WAGS.Lib.SFofT where

import Prelude
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<))
import Control.Semigroupoid (composeFlipped)
import Data.Lens (_1, _2, over, traversed)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\), type (/\))
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter, AudioParameter_(..))
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroom :: Number }

type SAPFofT
  = TimeHeadroom -> Cofree ((->) TimeHeadroom) AudioParameter

-- | From a non-empty list of times and values, make a cofree comonad that emits audio parameters
-- | the first number is how much time to add when looping
-- | Based on a current time and a look-ahead
makeLoopingPiecewise :: Number -> NonEmpty List (Number /\ Number) -> SAPFofT
makeLoopingPiecewise v0 v1 = go v0 v1 v1
  where
  go n i (a /\ b :| Nil) th =
    let
      l@(l' :| l'') = over (traversed <<< _1) (add n) i
    in
      go n l (a /\ b :| (l' : l'')) th

  go n l v@(a /\ b :| (Cons (c /\ d) e)) { time, headroom }
    | time >= a && time < c =
      let
        lookahead = time + headroom
      in
        ( if lookahead >= c then
            AudioParameter
              { param: Just d
              , timeOffset: c - time
              , transition: LinearRamp
              }
          else
            AudioParameter { param: Just (calcSlope a b c d time), timeOffset: 0.0, transition: LinearRamp }
        )
          :< go n l v
    | otherwise = go n l (c /\ d :| e) { time, headroom }

infixl 6 makeLoopingPiecewise as /@:<

-- | From a non-empty list of times and values, make a cofree comonad that emits audio parameters
-- | Based on a current time and a look-ahead
makePiecewise :: NonEmpty List (Number /\ Number) -> SAPFofT
makePiecewise (a /\ b :| Nil) _ =
  AudioParameter
    { param: Just b
    , timeOffset: 0.0
    , transition: LinearRamp
    }
    :< makePiecewise (a /\ b :| Nil)

makePiecewise v@(a /\ b :| (Cons (c /\ d) e)) { time, headroom }
  | time >= a && time < c =
    let
      lookahead = time + headroom
    in
      ( if lookahead >= c then
          AudioParameter
            { param: Just d
            , timeOffset: c - time
            , transition: LinearRamp
            }
        else
          AudioParameter { param: Just (calcSlope a b c d time), timeOffset: 0.0, transition: LinearRamp }
      )
        :< makePiecewise v
  | otherwise = makePiecewise (c /\ d :| e) { time, headroom }

infixl 6 makePiecewise as /:<

type SFofT a b
  = { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b

type SFofT' a
  = Number -> Cofree ((->) Number) a

-- | From a non-empty list, create a streaming function of time
nonEmptyToSFofTFull :: forall a b. Maybe ({ time :: Number, value :: a } -> b) -> NonEmpty List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToSFofTFull maybeOtherwise (h :| t) = go (h : t)
  where
  go :: List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
  go Nil = case maybeOtherwise of
    Just f -> let q i = f i :< q in q
    Nothing -> go (h : t)

  go ((tf /\ vf) : b) = let q i@{ time } = if tf time then (vf i :< q) else go b i in q

infixl 6 nonEmptyToSFofTFull as :!:<

nonEmptyToSFofT' :: forall a. Maybe a -> NonEmpty List ((Number -> Boolean) /\ a) -> Number -> Cofree ((->) Number) a
nonEmptyToSFofT' a b c =
  hoistCofree (\ftu n -> ftu { time: n, value: unit })
    (nonEmptyToSFofT (map pure a) (map (over _2 pure) b) { time: c, value: unit })

infixl 6 nonEmptyToSFofT' as :-:<

nonEmptyToSFofT :: forall a b. Maybe (a -> b) -> NonEmpty List ((Number -> Boolean) /\ (a -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToSFofT a b =
  nonEmptyToSFofTFull
    (map (composeFlipped _.value) a)
    (map (over _2 (composeFlipped _.value)) b)

infixl 6 nonEmptyToSFofT' as ::<

module WAGS.Lib.Learn.Note where

import Prelude

import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (Lens', _Just, lens, over)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Math (pow)
import Safe.Coerce (coerce)
import WAGS.Lib.Learn.Duration (Duration(..))
import WAGS.Lib.Learn.Pitch (Pitch)
import WAGS.Lib.Learn.Volume (Volume)

data Note = Note { volume :: Volume, duration :: Duration, pitch :: Pitch } | Rest { duration :: Duration }

class IntableIndex a where
  indexToInt :: a -> Int

instance intableIndexInt :: IntableIndex Int where
  indexToInt = identity

instance intableIndexMaybeInt :: IntableIndex (Maybe Int) where
  indexToInt = maybe 0 (add 1)

note :: Volume -> Duration -> Pitch -> Note
note v d p = Note { volume: v, duration: d, pitch: p }

volume :: forall p b. Strong p => Choice p => p Volume b -> p Note Note
volume =
  lens
    ( case _ of
        Note { volume: v } -> Just v
        Rest _ -> Nothing
    )
    const <<< _Just

duration :: Lens' Note Duration
duration = lens
  ( case _ of
      Note { duration: d } -> d
      Rest { duration: d } -> d
  )
  ( case _ of
      Note i -> Note <<< i { duration = _ }
      Rest i -> Rest <<< i { duration = _ }
  )

pitch :: forall p b. Strong p => Choice p => p Pitch b -> p Note Note
pitch =
  lens
    ( case _ of
        Note { pitch: p } -> Just p
        Rest _ -> Nothing
    )
    const <<< _Just

accelerando :: forall f i. IntableIndex i => FunctorWithIndex i f => f Note -> f Note
accelerando = mapWithIndex (over duration <<< mul <<< coerce <<< div 1.0 <<< pow 0.2 <<< toNumber <<< indexToInt)

rallentando :: forall f i. IntableIndex i => FunctorWithIndex i f => f Note -> f Note
rallentando = mapWithIndex (over duration <<< mul <<< coerce <<< pow 0.35 <<< toNumber <<< indexToInt)

transpose :: Pitch -> Note -> Note
transpose = over pitch <<< add
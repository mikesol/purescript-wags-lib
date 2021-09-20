module WAGS.Lib.Learn.Note where

import Prelude

import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (Lens', over)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Math (pow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration, Gap(..))
import WAGS.Lib.Learn.Pitch (Pitch)
import WAGS.Lib.Learn.Volume (Volume)

newtype Note = Note { startsAfter :: Gap, volume :: Volume, duration :: Duration, pitch :: Pitch }

derive instance newtypeNote :: Newtype Note _

class IntableIndex a where
  indexToInt :: a -> Int

instance intableIndexInt :: IntableIndex Int where
  indexToInt = identity

instance intableIndexMaybeInt :: IntableIndex (Maybe Int) where
  indexToInt = maybe 0 (add 1)

note :: Gap -> Volume -> Duration -> Pitch -> Note
note s v d p = Note { startsAfter: s, volume: v, duration: d, pitch: p }

volume :: Lens' Note Volume
volume = unto Note <<< prop (Proxy :: _ "volume")

pitch :: Lens' Note Pitch
pitch = unto Note <<< prop (Proxy :: _ "pitch")

duration :: Lens' Note Duration
duration = unto Note <<< prop (Proxy :: _ "duration")

startsAfter :: Lens' Note Gap
startsAfter = unto Note <<< prop (Proxy :: _ "startsAfter")

accelerando :: forall f i. IntableIndex i => FunctorWithIndex i f => f Note -> f Note
accelerando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< div 1.0 <<< pow 0.2 <<< toNumber <<< indexToInt)

rallentando :: forall f i. IntableIndex i => FunctorWithIndex i f => f Note -> f Note
rallentando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< pow 0.35 <<< toNumber <<< indexToInt)

transpose :: Pitch -> Note -> Note
transpose = over pitch <<< add
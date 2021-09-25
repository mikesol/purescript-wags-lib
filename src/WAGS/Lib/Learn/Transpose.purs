module WAGS.Lib.Learn.Transpose where

import Prelude

import Data.Lens (over)
import Data.Newtype (unwrap)
import Data.Variant (case_, on)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Note (Note, NoteOrRest, nt, rs, pitch)
import WAGS.Lib.Learn.Pitch (Pitch)

class Transpose f n where
  transpose :: Pitch f -> n -> n

instance transposePitch :: Applicative f => Transpose f (Pitch f) where
  transpose = add

instance transposeNote :: Applicative f => Transpose f (Note volumeF durationF f) where
  transpose = over pitch <<< transpose

instance transposeNoteWithRest :: Applicative f => Transpose f (NoteOrRest volumeF durationF f restF) where
  transpose val =
    ( case_
        # on (Proxy :: _ "note") (nt <<< transpose val)
        # on (Proxy :: _ "rest") rs
    )
      <<< unwrap

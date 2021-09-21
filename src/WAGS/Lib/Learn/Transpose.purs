module WAGS.Lib.Learn.Transpose where

import Prelude

import Data.Lens (over)
import Data.Newtype (unwrap)
import Data.Variant (match)
import WAGS.Lib.Learn.Note (Note, NoteOrRest, nt, rs, pitch)
import WAGS.Lib.Learn.Pitch (Pitch)

class Transpose n where
  transpose :: Pitch -> n -> n

instance transposePitch :: Transpose Pitch where
  transpose = add

instance transposeNote :: Transpose Note where
  transpose = over pitch <<< transpose

instance transposeNoteWithRest :: Transpose NoteOrRest where
  transpose val =
    match
      { note: nt <<< transpose val
      , rest: rs
      } <<< unwrap

module WAGS.Lib.Learn.Transpose where

import Prelude

import Data.Lens (over)
import WAGS.Lib.Learn.Note (Note, NoteWithRest, pitch, toNote)
import WAGS.Lib.Learn.Pitch (Pitch)

class Transpose n where
  transpose :: Pitch -> n -> n

instance transposePitch :: Transpose Pitch where
  transpose = add

instance transposeNote :: Transpose Note where
  transpose = over pitch <<< transpose

instance transposeNoteWithRest :: Transpose NoteWithRest where
  transpose = over toNote <<< transpose

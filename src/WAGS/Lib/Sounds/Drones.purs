module WAGS.Lib.Sounds.Drones where

import Prelude

import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object, fromHomogeneous)
import Foreign.Object as Object
import WAGS.Lib.Tidal.Types (BufferUrl(..), Sample(..))

dronesRaw :: Object String
dronesRaw = fromHomogeneous
  { "spacewind:0": "https://freesound.org/data/previews/370/370754_3104030-hq.mp3"
  , "ambienta:0": "https://freesound.org/data/previews/546/546360_10196790-hq.mp3"
  , "lowdark:0": "https://freesound.org/data/previews/579/579260_10522382-hq.mp3"
  , "harmonium:0": "https://freesound.org/data/previews/264/264442_4965426-hq.mp3"
  , "hollowair:0": "https://freesound.org/data/previews/370/370316_3104030-hq.mp3"
  , "digeridoo:0": "https://freesound.org/data/previews/197/197998_3684181-hq.mp3"
  }

drones :: Map Sample BufferUrl
drones = Map.fromFoldable
  $ map (bimap Sample BufferUrl)
  $ (Object.toUnfoldable :: _ -> Array (String /\ String)) dronesRaw
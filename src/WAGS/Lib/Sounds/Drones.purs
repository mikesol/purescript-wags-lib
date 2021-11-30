module WAGS.Lib.Sounds.Drones where

import Prelude

import Foreign.Object (Object, fromHomogeneous)
import WAGS.Lib.Tidal.Types (BufferUrl(..))

dronesRaw :: Object String
dronesRaw = fromHomogeneous
  { "spacewind:0": "https://media.graphcms.com/huYqv3YTRp2897eaReSl"
  , "ambienta:0": "https://media.graphcms.com/dOsnH4XSHaYxut8hT7lQ"
  , "lowdark:0": "https://media.graphcms.com/x9aA1yAgRwCCBdzYBNIj"
  , "harmonium:0": "https://media.graphcms.com/5uiXrX2IQW2hoktWAowd"
  , "hollowair:0": "https://media.graphcms.com/eG3qD6c5SEaTia8m5s4O"
  , "digeridoo:0": "https://media.graphcms.com/FeqmdhiQVjqrQfpxoHVw"
  }

drones :: Object BufferUrl
drones = map BufferUrl $ dronesRaw
module Main where

import Prelude

import Effect (Effect)
import WAGS.Lib.Learn (play)
import WAGS.Lib.Learn.Pitch (middleC)

main :: Effect Unit
main = play middleC
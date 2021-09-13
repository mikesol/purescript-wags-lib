module WAGS.Lib.Learn where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Effect (Effect)

playInt :: Int -> Effect Unit
playInt = const $ pure unit

playNumber :: Number -> Effect Unit
playNumber = const $ pure unit

playArrayInt :: Array Int -> Effect Unit
playArrayInt = const $ pure unit

playArrayNumber :: Array Number -> Effect Unit
playArrayNumber = const $ pure unit

playCofreeInt :: Cofree Identity Int -> Effect Unit
playCofreeInt = const $ pure unit

playCofreeNumber :: Cofree Identity Number -> Effect Unit
playCofreeNumber = const $ pure unit

playFunctionOfTimeInt :: (Number -> Int) -> Effect Unit
playFunctionOfTimeInt = const $ pure unit

playFunctionOfTimeNumber :: (Number -> Number) -> Effect Unit
playFunctionOfTimeNumber = const $ pure unit

playArrayFunctionOfTimeInt :: Array (Number -> Int) -> Effect Unit
playArrayFunctionOfTimeInt = const $ pure unit

playArrayFunctionOfTimeNumber :: Array (Number -> Number) -> Effect Unit
playArrayFunctionOfTimeNumber = const $ pure unit

playCofreeFunctionOfTimeInt :: Cofree ((->) Number) Int -> Effect Unit
playCofreeFunctionOfTimeInt = const $ pure unit

playCofreeFunctionOfTimeNumber :: Cofree ((->) Number) Number -> Effect Unit
playCofreeFunctionOfTimeNumber = const $ pure unit

-- nested tuple of playables
-- nested tuple of playables FoT
-- record of playables
-- record of playables FoT
-- graph
-- graph FoT
-- withBuffer
-- withPeriodicOsc
-- withControl
-- all of this as component
module WAGS.Lib.Trigger where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap, wrap)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Cofree (combineComonadCofreeChooseB)
import WAGS.Lib.Emitter (fEmitter)

type TimeOffset
  = { time :: Number, offset :: Maybe Number }

type TimeHeadroomFreq
  = { time :: Number, headroomInSeconds :: Number, freq :: Number }

type MakeTriggerWithOffset a
  = TimeOffset -> a

type MakeSnappyTrigger a
  = TimeHeadroomFreq -> a

type CfTrigger
  = Cofree ((->) TimeOffset) Time

type Time
  = Maybe (Additive Number)

type ATrigger
  = MakeTriggerWithOffset CfTrigger

type CfSnappyTrigger = Cofree ((->) TimeHeadroomFreq) Time

type ASnappyTrigger
  = MakeSnappyTrigger CfSnappyTrigger

makeTrigger :: ATrigger
makeTrigger = go Nothing
  where
  go :: Maybe Number -> TimeOffset -> CfTrigger
  go _ { time, offset: Just t } = let newT = time + t in Just (wrap (-t)) :< go (Just newT)

  go Nothing { offset: Nothing } = Nothing :< go Nothing

  go (Just t) { time, offset: Nothing } = let newT = time - t in Just (wrap newT) :< go (Just t)

makeSnappyTrigger :: ASnappyTrigger
makeSnappyTrigger = combineComonadCofreeChooseB
  ( \cont e b ({ time, headroomInSeconds, freq } :: TimeHeadroomFreq) ->
      let
        emitted = fEmitter freq { time, headroomInSeconds }
        enow = e (isJust emitted)
        bnow =
          b
            { time
            , offset: if unwrap (extract enow) then emitted else Nothing
            }
      in
        cont enow bnow
  )
  makeBlip
  makeTrigger


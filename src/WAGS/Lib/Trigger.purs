module WAGS.Lib.Trigger where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap, wrap)
import WAGS.Lib.Blip (ABlip, makeBlip)
import WAGS.Lib.Emitter (fEmitter)

type TimeOffset
  = { time :: Number, offset :: Maybe Number }

type TimeHeadroomFreq
  = { time :: Number, headroom :: Number, freq :: Number }

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
makeSnappyTrigger = go blip trigger
  where
  blip = makeBlip

  trigger = makeTrigger

  go :: ABlip -> ATrigger -> TimeHeadroomFreq -> CfSnappyTrigger
  go e b { time, headroom, freq } = extract bnow :< go (unwrapCofree enow) (unwrapCofree bnow)
    where
    emitted = fEmitter freq { time, headroom }

    enow = e (isJust emitted)

    bnow =
      b
        { time
        , offset: if unwrap (extract enow) then emitted else Nothing
        }

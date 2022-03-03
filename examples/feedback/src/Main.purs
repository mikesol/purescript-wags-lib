module Main where

import Prelude

import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (create)
import Feedback.App as App
import Feedback.PubNub (pubnubEvent)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    pne <- liftEffect pubnubEvent
    localEvent <- liftEffect create
    runUI (App.component (fst pne) localEvent (snd pne)) unit body

module Main where

import Prelude

import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Feedback.App as App
import Feedback.PubNub (pubnubEvent)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    pne <- liftEffect pubnubEvent
    runUI (App.component (fst pne) (snd pne)) unit body

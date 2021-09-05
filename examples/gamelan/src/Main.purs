module Main where

import Prelude

import Effect (Effect)
import Gamelan.App as App
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI App.component unit body

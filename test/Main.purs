module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.BufferPool (testBufferPool)
import Test.Rate (testRate)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Stream (testStream)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        testRate
        testBufferPool
        testStream

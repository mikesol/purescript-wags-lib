module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Blip (testBlip)
import Test.BufferPool (testBufferPool)
import Test.Emitter (testEmitter)
import Test.Impulse (testImpulse)
import Test.Lag (testLag)
import Test.Latch (testLatch)
import Test.Piecewise (testPiecewise)
import Test.Rate (testRate)
import Test.Record (testRecord)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Stream (testStream)
import Test.Trigger (testTrigger)
import Test.Vec (testMapWithTypedIndex)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        testRecord
        testRate
        testBufferPool
        testStream
        testImpulse
        testEmitter
        testBlip
        testPiecewise
        testTrigger
        testLatch
        testLag
        testMapWithTypedIndex

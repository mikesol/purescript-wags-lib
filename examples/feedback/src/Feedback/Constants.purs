module Feedback.Constants where

import Prelude

import Data.Array (drop)
import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty)
import Data.NonEmpty ((:|))
import Data.Unfoldable (replicate)

alwaysOpen :: NonEmptyArray Number
alwaysOpen = fromNonEmpty $ 1.0 :| [ 1.0 ]

-- U means uneven
-- this is for gains
longLFOU3 :: NonEmptyArray Number
longLFOU3 = fromNonEmpty
  $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.0, 0.66, 1.0, 0.66 ])

longLFOU2 :: NonEmptyArray Number
longLFOU2 = fromNonEmpty
  $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.35, 0.82, 1.0, 0.82 ])

longLFOU1 :: NonEmptyArray Number
longLFOU1 = fromNonEmpty
  $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.6, 0.9, 1.0, 0.9 ])

longLFOU0 :: NonEmptyArray Number
longLFOU0 = fromNonEmpty
  $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.8, 0.95, 1.0, 0.95 ])

-- drone bandpass 0
droneBandpass0LFO :: NonEmptyArray Number
droneBandpass0LFO = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 300.0, 600.0, 900.0, 600.0 ])

droneBandpass1LFO :: NonEmptyArray Number
droneBandpass1LFO = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 1200.0, 1600.0, 2000.0, 1600.0 ])

dropeHighpass1LFO :: NonEmptyArray Number
dropeHighpass1LFO = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 2000.0, 3000.0, 4000.0, 3000.0 ])

lfoTimeOffset :: Number
lfoTimeOffset = 0.3

waveshaperTimeOffset :: Number
waveshaperTimeOffset = 1.0

padFundamental = 83.0 :: Number
padOsc0Vol = 0.05 :: Number
padOsc0Duration = 4.0 :: Number
padOsc0Freq = padFundamental :: Number
padOsc1Vol = 0.05 :: Number
padOsc1Duration = 4.0 :: Number
padOsc1Freq = padFundamental * 2.0 :: Number
padOsc2Vol = 0.05 :: Number
padOsc2Duration = 4.0 :: Number
padOsc2Freq = padFundamental * 3.0 :: Number
padOsc3Vol = 0.05 :: Number
padOsc3Duration = 4.0 :: Number
padOsc3Freq = padFundamental * 5.0 :: Number
padOsc4Vol = 0.05 :: Number
padOsc4Duration = 4.0 :: Number
padOsc4Freq = padFundamental * 8.0 :: Number

detuneDuration :: Number
detuneDuration = 2.0

--
padFilter0Freq :: Number
padFilter0Freq = 120.0

padFilter1Freq :: Number
padFilter1Freq = 420.0

padFilter2Freq :: Number
padFilter2Freq = 1050.0

padFilter3Freq :: Number
padFilter3Freq = 2010.0

padFilter0Q :: Number
padFilter0Q = 1.0

padFilter1Q :: Number
padFilter1Q = 2.0

padFilter2Q :: Number
padFilter2Q = 5.0

padFilter3Q :: Number
padFilter3Q = 10.0

--
leadDelay0Duration :: Number
leadDelay0Duration = 2.5

leadDelay1Duration :: Number
leadDelay1Duration = 2.5

leadDelay2Duration :: Number
leadDelay2Duration = 2.5

leadDelayCombinedDuration :: Number
leadDelayCombinedDuration = 2.5

leadDelaySliderDuration :: Number
leadDelaySliderDuration = 0.05
--
droneFade :: Number
droneFade = 3.0

droneFlangeVol :: Number
droneFlangeVol = 0.3

droneFlangeFadeDuration :: Number
droneFlangeFadeDuration = 3.1416
--
sampleDelay0Duration :: Number
sampleDelay0Duration = 2.5

sampleDelay1Duration :: Number
sampleDelay1Duration = 2.5

sampleDelay2Duration :: Number
sampleDelay2Duration = 2.5

sampleDelayCombinedDuration :: Number
sampleDelayCombinedDuration = 2.5

sampleDelaySliderDuration :: Number
sampleDelaySliderDuration = 0.05

---
loopingBuffer0Duration :: Number
loopingBuffer0Duration = 2.5

loopingBuffer1Duration :: Number
loopingBuffer1Duration = 2.5

loopingBuffer2Duration :: Number
loopingBuffer2Duration = 2.5

loopingBuffer3Duration :: Number
loopingBuffer3Duration = 2.5

loopingBuffer4Duration :: Number
loopingBuffer4Duration = 2.5

loopingBufferCombinedDuration :: Number
loopingBufferCombinedDuration = 2.5

loopingBufferSliderDuration :: Number
loopingBufferSliderDuration = 0.05
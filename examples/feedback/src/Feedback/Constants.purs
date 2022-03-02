module Feedback.Constants where

import Prelude

import Data.Array (drop)
import Data.NonEmpty ((:|))
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Maybe (fromMaybe)
import Data.Unfoldable (replicate)

-- U means uneven
longLFOU3 = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.0, 0.66, 1.0, 0.66 ])
longLFOU2 = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.35, 0.82, 1.0, 0.82 ])
longLFOU1 = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.6, 0.9, 1.0, 0.9 ])
longLFOU0 = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.8, 0.95, 1.0, 0.95 ])

-- E means even
longLFOE0 = fromNonEmpty $ 0.0 :| (drop 1 $ join $ replicate 10000 [ 0.0, 0.5, 1.0, 0.5 ])

lfoTimeOffset = 0.3
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
detuneDuration = 2.0
--
padFilter0Freq = 120.0
padFilter1Freq = 420.0
padFilter2Freq = 1050.0
padFilter3Freq = 2010.0
padFilter0Q = 1.0
padFilter1Q = 2.0
padFilter2Q = 5.0
padFilter3Q = 10.0
--
leadDelay0Duration = 2.5
leadDelay1Duration = 2.5
leadDelay2Duration = 2.5
leadDelayCombinedDuration = 2.5
leadDelaySliderDuration = 0.05

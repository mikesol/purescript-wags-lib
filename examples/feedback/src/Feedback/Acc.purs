module Feedback.Acc where

import Prelude

import CallByName.Applicative as CBNA
import Data.Array ((..))
import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Feedback.Types (Acc, EnvelopeType(..), LeadSynth(..), OctaveType(..), PitchSynth(..), SampleRate(..), Synths, TriggerLeadInfo, TriggerLeadNT(..), TriggerOneShotInfo, TriggerOneShotNT(..), UncontrollableNT(..), WhichSample(..), ZeroToOne(..))
import WAGS.Change (ichange)
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioOnOff(..), AudioParameter, _offOn, envelope)
import WAGS.Lib.Piecewise (simplePiecewise)
import WAGS.Lib.Stream (cycleL)
import WAGS.WebAPI (BrowserAudioBuffer)

synth2prefix :: LeadSynth -> String
synth2prefix = case _ of
  Synth0 -> "synth0"
  Synth1 -> "synth1"
  Synth2 -> "synth2"
  Synth3 -> "synth3"
  Synth4 -> "synth4"

-- todo: this could be a lot simpler if it is really this regular,
-- but the nice thing about this is that it allows for aribtrary sequences
-- if we ever want to go down that route

ps :: PitchSynth -> Int -> Synths -> BrowserAudioBuffer
ps = case _ of
  P0 -> case _ of
    0 -> _.p0
    1 -> _.p1
    2 -> _.p2
    3 -> _.p3
    4 -> _.p4
    5 -> _.p5
    6 -> _.p6
    _ -> _.p7
  P1 -> case _ of
    0 -> _.p1
    1 -> _.p2
    2 -> _.p3
    3 -> _.p4
    4 -> _.p5
    5 -> _.p6
    6 -> _.p7
    _ -> _.p8
  P2 -> case _ of
    0 -> _.p2
    1 -> _.p3
    2 -> _.p4
    3 -> _.p5
    4 -> _.p4
    5 -> _.p5
    6 -> _.p6
    _ -> _.p9
  P3 -> case _ of
    0 -> _.p3
    1 -> _.p4
    2 -> _.p5
    3 -> _.p6
    4 -> _.p7
    5 -> _.p8
    6 -> _.p9
    _ -> _.p10
  P4 -> case _ of
    0 -> _.p4
    1 -> _.p5
    2 -> _.p6
    3 -> _.p7
    4 -> _.p8
    5 -> _.p9
    6 -> _.p10
    _ -> _.p11
  P5 -> case _ of
    0 -> _.p5
    1 -> _.p6
    2 -> _.p7
    3 -> _.p8
    4 -> _.p9
    5 -> _.p10
    6 -> _.p11
    _ -> _.p12
  P6 -> case _ of
    0 -> _.p6
    1 -> _.p7
    2 -> _.p8
    3 -> _.p9
    4 -> _.p10
    5 -> _.p11
    6 -> _.p12
    _ -> _.p13
  P7 -> case _ of
    0 -> _.p7
    1 -> _.p8
    2 -> _.p9
    3 -> _.p10
    4 -> _.p11
    5 -> _.p12
    6 -> _.p13
    _ -> _.p0
  P8 -> case _ of
    0 -> _.p8
    1 -> _.p9
    2 -> _.p10
    3 -> _.p11
    4 -> _.p12
    5 -> _.p13
    6 -> _.p0
    _ -> _.p1
  P9 -> case _ of
    0 -> _.p9
    1 -> _.p10
    2 -> _.p11
    3 -> _.p12
    4 -> _.p13
    5 -> _.p0
    6 -> _.p1
    _ -> _.p2
  P10 -> case _ of
    0 -> _.p10
    1 -> _.p11
    2 -> _.p12
    3 -> _.p13
    4 -> _.p0
    5 -> _.p1
    6 -> _.p2
    _ -> _.p3
  P11 -> case _ of
    0 -> _.p11
    1 -> _.p12
    2 -> _.p13
    3 -> _.p0
    4 -> _.p1
    5 -> _.p2
    6 -> _.p3
    _ -> _.p4
  P12 -> case _ of
    0 -> _.p12
    1 -> _.p13
    2 -> _.p0
    3 -> _.p1
    4 -> _.p2
    5 -> _.p3
    6 -> _.p4
    _ -> _.p5
  P13 -> case _ of
    0 -> _.p13
    1 -> _.p0
    2 -> _.p1
    3 -> _.p2
    4 -> _.p3
    5 -> _.p4
    6 -> _.p5
    _ -> _.p6

makeLeadBufferInstruction
  :: TriggerLeadInfo
  -> { onOff :: AudioOnOff
     , buffer :: BrowserAudioBuffer
     , playbackRate :: Number
     }
makeLeadBufferInstruction { n, nPitches, octaveLead, synthPitchProfile, synthPrefix, buffers } =
  { onOff: AudioOnOff
      { onOff: _offOn
      , timeOffset: toNumber n * 1.75 / toNumber nPitches
      }
  , playbackRate: case octaveLead of
      Oct0 -> 1.0
      Oct1 -> 1.3
      Oct2 -> 0.9
  , buffer: case synthPrefix of
      Synth0 -> buffers.synths.synth0 # ps synthPitchProfile n
      Synth1 -> buffers.synths.synth1 # ps synthPitchProfile n
      Synth2 -> buffers.synths.synth2 # ps synthPitchProfile n
      Synth3 -> buffers.synths.synth3 # ps synthPitchProfile n
      Synth4 -> buffers.synths.synth4 # ps synthPitchProfile n
  }

mpw :: (Number -> Number) -> Int -> NonEmptyArray Number
mpw ff ii =
  let
    iin = toNumber ii
    fff = lcmap ((_ / iin) <<< toNumber) ff
  in
    fromNonEmpty (fff 0 :| map fff (1 .. (ii - 1)))

env0 :: Number -> Number
env0 = simplePiecewise [ 0.0 /\ 0.0, 0.1 /\ 1.0, 0.2 /\ 0.3, 0.8 /\ 0.0 ]

env1 :: Number -> Number
env1 = simplePiecewise [ 0.0 /\ 0.0, 0.25 /\ 1.0, 0.5 /\ 0.3, 1.0 /\ 0.0 ]

env2 :: Number -> Number
env2 = simplePiecewise [ 0.0 /\ 0.0, 0.3 /\ 0.3, 0.75 /\ 1.0, 0.9 /\ 0.0 ]

makeLeadGainInstruction :: TriggerLeadInfo -> AudioParameter
makeLeadGainInstruction = _.envType
  >>>
    ( case _ of
        Env0 -> AudioEnvelope { values: mpw env0 128, duration: 1.0, timeOffset: 0.0 }
        Env1 -> AudioEnvelope { values: mpw env1 128, duration: 1.0, timeOffset: 0.0 }
        Env2 -> AudioEnvelope { values: mpw env2 128, duration: 1.0, timeOffset: 0.0 }
    )
  >>> envelope

makeSampleBufferInstruction
  :: TriggerOneShotInfo
  -> { onOff :: AudioOnOff
     , buffer :: BrowserAudioBuffer
     , playbackRate :: Number
     }
makeSampleBufferInstruction
  { sampleReverse
  , sampleChooser
  , sampleRateChange
  , buffers
  } =
  { onOff: AudioOnOff
      { onOff: _offOn
      , timeOffset: 0.0
      }
  , playbackRate: case sampleRateChange of
      Sr0 -> 1.0
      Sr1 -> 1.1
      Sr2 -> 1.4
  , buffer: case sampleChooser of
      Sm0 -> if sampleReverse then buffers.oneShotsBackwards.o0 else buffers.oneShots.o0
      Sm1 -> if sampleReverse then buffers.oneShotsBackwards.o1 else buffers.oneShots.o1
      Sm2 -> if sampleReverse then buffers.oneShotsBackwards.o2 else buffers.oneShots.o2
      Sm3 -> if sampleReverse then buffers.oneShotsBackwards.o3 else buffers.oneShots.o3
      Sm4 -> if sampleReverse then buffers.oneShotsBackwards.o4 else buffers.oneShots.o4
      Sm5 -> if sampleReverse then buffers.oneShotsBackwards.o5 else buffers.oneShots.o5
      Sm6 -> if sampleReverse then buffers.oneShotsBackwards.o6 else buffers.oneShots.o6
      Sm7 -> if sampleReverse then buffers.oneShotsBackwards.o7 else buffers.oneShots.o7
      Sm8 -> if sampleReverse then buffers.oneShotsBackwards.o8 else buffers.oneShots.o8
      Sm9 -> if sampleReverse then buffers.oneShotsBackwards.o9 else buffers.oneShots.o9
      Sm10 -> if sampleReverse then buffers.oneShotsBackwards.o10 else buffers.oneShots.o10
      Sm11 -> if sampleReverse then buffers.oneShotsBackwards.o11 else buffers.oneShots.o11
      Sm12 -> if sampleReverse then buffers.oneShotsBackwards.o12 else buffers.oneShots.o12
      Sm13 -> if sampleReverse then buffers.oneShotsBackwards.o13 else buffers.oneShots.o13
      Sm14 -> if sampleReverse then buffers.oneShotsBackwards.o14 else buffers.oneShots.o14
      Sm15 -> if sampleReverse then buffers.oneShotsBackwards.o15 else buffers.oneShots.o15
      Sm16 -> if sampleReverse then buffers.oneShotsBackwards.o16 else buffers.oneShots.o16
      Sm17 -> if sampleReverse then buffers.oneShotsBackwards.o17 else buffers.oneShots.o17
      Sm18 -> if sampleReverse then buffers.oneShotsBackwards.o18 else buffers.oneShots.o18
      Sm19 -> if sampleReverse then buffers.oneShotsBackwards.o19 else buffers.oneShots.o19
      Sm20 -> if sampleReverse then buffers.oneShotsBackwards.o20 else buffers.oneShots.o20
      Sm21 -> if sampleReverse then buffers.oneShotsBackwards.o21 else buffers.oneShots.o21
      Sm22 -> if sampleReverse then buffers.oneShotsBackwards.o22 else buffers.oneShots.o22
      Sm23 -> if sampleReverse then buffers.oneShotsBackwards.o23 else buffers.oneShots.o23
  }

makeSampleGainInstruction :: TriggerOneShotInfo -> Number
makeSampleGainInstruction _ = 1.0

makeChorusBufferInstruction
  :: TriggerOneShotInfo
  -> { onOff :: AudioOnOff
     , buffer :: BrowserAudioBuffer
     , playbackRate :: Number
     }
makeChorusBufferInstruction a = base
  { playbackRate = case a.sampleRateChange of
      Sr0 -> 1.02
      Sr1 -> 1.13
      Sr2 -> 1.45
  }
  where
  base = makeSampleBufferInstruction a

makeChorusGainInstruction :: TriggerOneShotInfo -> Number
makeChorusGainInstruction _ = 0.1

initialAcc :: Acc
initialAcc =
  { triggerLead: cycleL
      ( TriggerLeadNT
          ( \a -> ichange
              { leadSource0Buf: makeLeadBufferInstruction a
              , leadSource0: makeLeadGainInstruction a
              }
          )
          :|
            TriggerLeadNT
              ( \a -> ichange
                  { leadSource1Buf: makeLeadBufferInstruction a
                  , leadSource1: makeLeadGainInstruction a
                  }
              )
              :
                TriggerLeadNT
                  ( \a -> ichange
                      { leadSource2Buf: makeLeadBufferInstruction a
                      , leadSource2: makeLeadGainInstruction a
                      }
                  )
              :
                TriggerLeadNT
                  ( \a -> ichange
                      { leadSource3Buf: makeLeadBufferInstruction a
                      , leadSource3: makeLeadGainInstruction a
                      }
                  )
              :
                TriggerLeadNT
                  ( \a -> ichange
                      { leadSource4Buf: makeLeadBufferInstruction a
                      , leadSource4: makeLeadGainInstruction a
                      }
                  )
              : Nil
      )
  , triggerOneShot: cycleL
      ( TriggerOneShotNT
          ( \a -> do
              ichange
                { smplrSource0Buf: makeSampleBufferInstruction a
                , smplrSource0: makeSampleGainInstruction a
                }
              CBNA.when a.sampleChorusEffect \_ -> ichange
                { smplrSource3Buf: makeChorusBufferInstruction a
                , smplrSource3: makeChorusGainInstruction a
                }

          )
          :|
            TriggerOneShotNT
              ( \a -> do
                  ichange
                    { smplrSource1Buf: makeSampleBufferInstruction a
                    , smplrSource1: makeSampleGainInstruction a
                    }
                  CBNA.when a.sampleChorusEffect \_ -> ichange
                    { smplrSource4Buf: makeChorusBufferInstruction a
                    , smplrSource4: makeChorusGainInstruction a
                    }

              )
              :
                TriggerOneShotNT
                  ( \a -> do
                      ichange
                        { smplrSource2Buf: makeSampleBufferInstruction a
                        , smplrSource2: makeSampleGainInstruction a
                        }
                      CBNA.when a.sampleChorusEffect \_ -> ichange
                        { smplrSource0Buf: makeChorusBufferInstruction a
                        , smplrSource0: makeChorusGainInstruction a
                        }

                  )
              :
                TriggerOneShotNT
                  ( \a -> do
                      ichange
                        { smplrSource3Buf: makeSampleBufferInstruction a
                        , smplrSource3: makeSampleGainInstruction a
                        }
                      CBNA.when a.sampleChorusEffect \_ -> ichange
                        { smplrSource1Buf: makeChorusBufferInstruction a
                        , smplrSource1: makeChorusGainInstruction a
                        }

                  )
              :
                TriggerOneShotNT
                  ( \a -> do
                      ichange
                        { smplrSource4Buf: makeSampleBufferInstruction a
                        , smplrSource4: makeSampleGainInstruction a
                        }
                      CBNA.when a.sampleChorusEffect \_ -> ichange
                        { smplrSource2Buf: makeChorusBufferInstruction a
                        , smplrSource2: makeChorusGainInstruction a
                        }

                  )
              : Nil
      )
  , synthPrefix: Synth0
  , synthPitchProfile: P0
  , nPitches: 1
  , envType: Env0
  , octaveLead: Oct0
  , droneActivationEnergyThreshold: ZeroToOne 0.0
  , droneDecay: ZeroToOne 0.0
  , leadDelayInfo:
      { leadDelayLine0: false
      , leadDelayLine1: false
      , leadDelayLine2: false
      , leadCombinedDelay0: false
      , leadDelayGainCarousel: ZeroToOne zero
      }
  , sampleReverse: false
  , sampleChooser: Sm0
  , sampleChorusEffect: false
  , sampleRateChange: Sr0
  , sampleDelayInfo:
      { sampleDelayLine0: false
      , sampleDelayLine1: false
      , sampleDelayLine2: false
      , sampleCombinedDelay0: false
      , sampleDelayGainCarousel: ZeroToOne zero
      }
  , loopingBufferInfo:
      { loopingBuffer0: false
      , loopingBuffer1: false
      , loopingBuffer2: false
      , loopingBuffer3: false
      , loopingBuffer4: false
      , loopingBufferGainDJ: false
      }
  , uncontrollable: cycleL
      ( UncontrollableNT
          (\_ -> ichange { uSingletonPlayBuf0: _offOn })
          :|
            UncontrollableNT
              (\_ -> ichange { uSingletonPlayBuf1: _offOn })
              :
                UncontrollableNT
                  (\_ -> ichange { uSingletonPlayBuf2: _offOn })
              :
                UncontrollableNT
                  (\_ -> ichange { uSingletonPlayBuf3: _offOn })
              :
                UncontrollableNT
                  (\_ -> ichange { uSingletonPlayBuf4: _offOn })
              : Nil
      )
  }
module Feedback.FullGraph where

import Prelude

import Data.Tuple.Nested (type (/\))
import WAGS.Graph.AudioUnit (OversampleTwoX, TBandpass, TDelay, TGain, THighpass, TLoopBuf, TLowpass, TPeriodicOsc(..), TPlayBuf, TSawtoothOsc(..), TSinOsc, TSpeaker, TSquareOsc(..), TStereoPanner, TTriangleOsc(..), TWaveShaper)

type FullGraph =
  ( speaker :: TSpeaker /\ { mainFader :: Unit }
  , mainFader ::
      TGain /\
        { subMainDelay :: Unit
        , subMainFader :: Unit
        }
  , subMainDelay :: TDelay /\ { subMainGain :: Unit }
  , subMainGain :: TGain /\ { mainFader :: Unit }
  , subMainFader ::
      TGain /\
        { loopingBuffer0 :: Unit
        , loopingBuffer1 :: Unit
        , loopingBuffer2 :: Unit
        , loopingBuffer3 :: Unit
        , loopingBuffer4 :: Unit
        , distantBells :: Unit
        , uSingleton :: Unit
        , pad :: Unit
        , drone :: Unit
        , smplr :: Unit
        , lead :: Unit
        }
  -- looping buffers
  , loopingBuffer0 :: TGain /\ { loopingBuffer0Pan :: Unit }
  , loopingBuffer0Pan :: TStereoPanner /\ { loopingBuffer0PlayBuf :: Unit }
  , loopingBuffer0PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer1 :: TGain /\ { loopingBuffer1Pan :: Unit }
  , loopingBuffer1Pan :: TStereoPanner /\ { loopingBuffer1PlayBuf :: Unit }
  , loopingBuffer1PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer2 :: TGain /\ { loopingBuffer2Pan :: Unit }
  , loopingBuffer2Pan :: TStereoPanner /\ { loopingBuffer2PlayBuf :: Unit }
  , loopingBuffer2PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer3 :: TGain /\ { loopingBuffer3Pan :: Unit }
  , loopingBuffer3Pan :: TStereoPanner /\ { loopingBuffer3PlayBuf :: Unit }
  , loopingBuffer3PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer4 :: TGain /\ { loopingBuffer4Pan :: Unit }
  , loopingBuffer4Pan :: TStereoPanner /\ { loopingBuffer4PlayBuf :: Unit }
  , loopingBuffer4PlayBuf :: TLoopBuf /\ {}
  -- bells
  , distantBells :: TGain /\ { distantBellsPan :: Unit }
  , distantBellsPan :: TStereoPanner /\ { distantBellsPlayBuf :: Unit }
  , distantBellsPlayBuf :: TLoopBuf /\ {}
  -- uncontrollable singleton
  , uSingleton :: TStereoPanner /\ { uSingletonGain :: Unit }
  , uSingletonGain ::
      TGain /\
        { uSingletonSource :: Unit
        , uSingletonHGain :: Unit
        }
  , uSingletonSource ::
      TGain /\
        { uSingletonPlayBuf0 :: Unit
        , uSingletonPlayBuf1 :: Unit
        , uSingletonPlayBuf2 :: Unit
        , uSingletonPlayBuf3 :: Unit
        , uSingletonPlayBuf4 :: Unit
        }
  , uSingletonPlayBuf0 :: TPlayBuf /\ {}
  , uSingletonPlayBuf1 :: TPlayBuf /\ {}
  , uSingletonPlayBuf2 :: TPlayBuf /\ {}
  , uSingletonPlayBuf3 :: TPlayBuf /\ {}
  , uSingletonPlayBuf4 :: TPlayBuf /\ {}
  , uSingletonHGain :: TGain /\ { uSingletonHHPF :: Unit }
  , uSingletonHHPF :: THighpass /\ { uSingletonHDelay :: Unit }
  , uSingletonHDelay :: TDelay /\ { uSingletonGain :: Unit }
  -- pad
  , pad ::
      TGain /\
        { padDry :: Unit
        , padWaveshaped :: Unit
        }
  , padDry :: TGain /\ { padMix :: Unit }
  , padWaveshaped :: TGain /\ { padWaveshaper :: Unit }
  , padWaveshaper :: TWaveShaper OversampleTwoX /\ { padMix :: Unit }
  , padMix ::
      TGain /\
        { padFilter0 :: Unit
        , padFilter1 :: Unit
        , padFilter2 :: Unit
        , padFilter3 :: Unit
        }
  , padFilter0 :: TBandpass /\ { padSources :: Unit }
  , padFilter1 :: TBandpass /\ { padSources :: Unit }
  , padFilter2 :: TBandpass /\ { padSources :: Unit }
  , padFilter3 :: TBandpass /\ { padSources :: Unit }
  , padSources ::
      TGain /\
        { padSource0 :: Unit
        , padSource1 :: Unit
        , padSource2 :: Unit
        , padSource3 :: Unit
        , padSource4 :: Unit
        }
  , padSource0 :: TGain /\ { padSource0LFO :: Unit }
  , padSource0LFO :: TGain /\ { padSource0Pan :: Unit }
  , padSource0Pan :: TStereoPanner /\ { padSource0Osc :: Unit }
  , padSource0Osc :: TPeriodicOsc /\ {}
  , padSource1 :: TGain /\ { padSource1LFO :: Unit }
  , padSource1LFO :: TGain /\ { padSource1Pan :: Unit }
  , padSource1Pan :: TStereoPanner /\ { padSource1Osc :: Unit }
  , padSource1Osc :: TTriangleOsc /\ {}
  , padSource2 :: TGain /\ { padSource2LFO :: Unit }
  , padSource2LFO :: TGain /\ { padSource2Pan :: Unit }
  , padSource2Pan :: TStereoPanner /\ { padSource2Osc :: Unit }
  , padSource2Osc :: TSawtoothOsc /\ {}
  , padSource3 :: TGain /\ { padSource3LFO :: Unit }
  , padSource3LFO :: TGain /\ { padSource3Pan :: Unit }
  , padSource3Pan :: TStereoPanner /\ { padSource3Osc :: Unit }
  , padSource3Osc :: TSquareOsc /\ {}
  , padSource4 :: TGain /\ { padSource4LFO :: Unit }
  , padSource4LFO :: TGain /\ { padSource4Pan :: Unit }
  , padSource4Pan :: TStereoPanner /\ { padSource4Osc :: Unit }
  , padSource4Osc :: TSinOsc /\ {}
  -- drone
  , drone :: TGain /\
        { droneFilter0 :: Unit
        , droneFilter1 :: Unit
        , droneFilter2 :: Unit
        , droneFilter3 :: Unit
        }
  , droneFilter0 :: TLowpass /\ { droneSources :: Unit }
  , droneFilter1 :: TBandpass /\ { droneSources :: Unit }
  , droneFilter2 :: TBandpass /\ { droneSources :: Unit }
  , droneFilter3 :: THighpass /\ { droneSources :: Unit }
  , droneSources ::
      TGain /\
        { droneSource0 :: Unit
        , droneSource1 :: Unit
        , droneSource2 :: Unit
        , droneSource3 :: Unit
        , droneSource4 :: Unit
        }
  , droneSource0 :: TGain /\ { droneSource0Buf :: Unit }
  , droneSource0Buf :: TLoopBuf /\ {}
  , droneSource1 :: TGain /\ { droneSource1Buf :: Unit }
  , droneSource1Buf :: TLoopBuf /\ {}
  , droneSource2 :: TGain /\ { droneSource2Buf :: Unit }
  , droneSource2Buf :: TLoopBuf /\ {}
  , droneSource3 :: TGain /\ { droneSource3Buf :: Unit }
  , droneSource3Buf :: TLoopBuf /\ {}
  , droneSource4 :: TGain /\ { droneSource4Buf :: Unit }
  , droneSource4Buf :: TLoopBuf /\ {}
  -- smplr
  , smplr :: TStereoPanner /\ { smplrMain :: Unit }
  , smplrMain :: TGain /\
        { smplrDelay0 :: Unit
        , smplrDelay1 :: Unit
        , smplrDelay2 :: Unit
        }
  ----
  , smplrDelay0 :: TGain /\ { smplrSourcesGain0 :: Unit, smplrDelay0Gain :: Unit }
  , smplrSourcesGain0 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay0Gain :: TGain /\ { smplrHighpass0 :: Unit }
  , smplrHighpass0 :: THighpass /\ { smplrDelayD0 :: Unit }
  , smplrDelayD0 :: TDelay /\ { smplrDelay0 :: Unit }
  ----
  , smplrDelay1 :: TGain /\ { smplrSourcesGain1 :: Unit, smplrDelay1Gain :: Unit }
  , smplrSourcesGain1 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay1Gain :: TGain /\ { smplrHighpass1 :: Unit }
  , smplrHighpass1 :: THighpass /\ { smplrDelayD1 :: Unit }
  , smplrDelayD1 :: TDelay /\ { smplrDelay1 :: Unit }
  ----
  , smplrDelay2 :: TGain /\ { smplrSourcesGain2 :: Unit, smplrDelay2Gain :: Unit }
  , smplrSourcesGain2 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay2Gain :: TGain /\ { smplrHighpass2 :: Unit }
  , smplrHighpass2 :: THighpass /\ { smplrDelayD2 :: Unit }
  , smplrDelayD2 :: TDelay /\ { smplrDelay2 :: Unit }
  ----
  , smplrDelayCbnd :: TGain /\ { smplrSourcesGainCbnd :: Unit
    , smplrDelayCbndGain :: Unit }
  , smplrSourcesGainCbnd :: TGain /\ { smplrDelay0 :: Unit, smplrDelay1 :: Unit }
  , smplrDelayCbndGain :: TGain /\ { smplrHighpassCbnd :: Unit }
  , smplrHighpassCbnd :: THighpass /\ { smplrDelayDCbnd :: Unit }
  , smplrDelayDCbnd :: TDelay /\ { smplrDelayCbnd :: Unit }
  ----
  , smplrSources ::
      TGain /\
        { smplrSource0 :: Unit
        , smplrSource1 :: Unit
        , smplrSource2 :: Unit
        , smplrSource3 :: Unit
        , smplrSource4 :: Unit
        }
  , smplrSource0 :: TGain /\ { smplrSource0Buf :: Unit }
  , smplrSource0Buf :: TPlayBuf /\ {}
  , smplrSource1 :: TGain /\ { smplrSource1Buf :: Unit }
  , smplrSource1Buf :: TPlayBuf /\ {}
  , smplrSource2 :: TGain /\ { smplrSource2Buf :: Unit }
  , smplrSource2Buf :: TPlayBuf /\ {}
  , smplrSource3 :: TGain /\ { smplrSource3Buf :: Unit }
  , smplrSource3Buf :: TPlayBuf /\ {}
  , smplrSource4 :: TGain /\ { smplrSource4Buf :: Unit }
  , smplrSource4Buf :: TPlayBuf /\ {}
  -- lead
  , lead :: TStereoPanner /\ { leadMain :: Unit }
  , leadMain :: TGain /\
        { leadDelay0 :: Unit
        , leadDelay1 :: Unit
        , leadDelay2 :: Unit
        }
  ----
  , leadDelay0 :: TGain /\ { leadSourcesGain0 :: Unit, leadDelay0Gain :: Unit }
  , leadSourcesGain0 :: TGain /\ { leadSources :: Unit }
  , leadDelay0Gain :: TGain /\ { leadHighpass0 :: Unit }
  , leadHighpass0 :: THighpass /\ { leadDelayD0 :: Unit }
  , leadDelayD0 :: TDelay /\ { leadDelay0 :: Unit }
  ----
  , leadDelay1 :: TGain /\ { leadSourcesGain1 :: Unit, leadDelay1Gain :: Unit }
  , leadSourcesGain1 :: TGain /\ { leadSources :: Unit }
  , leadDelay1Gain :: TGain /\ { leadHighpass1 :: Unit }
  , leadHighpass1 :: THighpass /\ { leadDelayD1 :: Unit }
  , leadDelayD1 :: TDelay /\ { leadDelay1 :: Unit }
  ----
  , leadDelay2 :: TGain /\ { leadSourcesGain2 :: Unit, leadDelay2Gain :: Unit }
  , leadSourcesGain2 :: TGain /\ { leadSources :: Unit }
  , leadDelay2Gain :: TGain /\ { leadHighpass2 :: Unit }
  , leadHighpass2 :: THighpass /\ { leadDelayD2 :: Unit }
  , leadDelayD2 :: TDelay /\ { leadDelay2 :: Unit }
  ----
  , leadDelayCbnd :: TGain /\ { leadSourcesGainCbnd :: Unit
    , leadDelayCbndGain :: Unit }
  , leadSourcesGainCbnd :: TGain /\ { leadDelay0 :: Unit, leadDelay1 :: Unit }
  , leadDelayCbndGain :: TGain /\ { leadHighpassCbnd :: Unit }
  , leadHighpassCbnd :: THighpass /\ { leadDelayDCbnd :: Unit }
  , leadDelayDCbnd :: TDelay /\ { leadDelayCbnd :: Unit }
  ----
  , leadSources ::
      TGain /\
        { leadSource0 :: Unit
        , leadSource1 :: Unit
        , leadSource2 :: Unit
        , leadSource3 :: Unit
        , leadSource4 :: Unit
        }
  , leadSource0 :: TGain /\ { leadSource0Buf :: Unit }
  , leadSource0Buf :: TPlayBuf /\ {}
  , leadSource1 :: TGain /\ { leadSource1Buf :: Unit }
  , leadSource1Buf :: TPlayBuf /\ {}
  , leadSource2 :: TGain /\ { leadSource2Buf :: Unit }
  , leadSource2Buf :: TPlayBuf /\ {}
  , leadSource3 :: TGain /\ { leadSource3Buf :: Unit }
  , leadSource3Buf :: TPlayBuf /\ {}
  , leadSource4 :: TGain /\ { leadSource4Buf :: Unit }
  , leadSource4Buf :: TPlayBuf /\ {}
  )

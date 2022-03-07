module Feedback.Setup where

import Prelude

import Feedback.Constants as C
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Trigger, World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (_on)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

setup :: forall proof. TriggeredScene Trigger World () -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
setup (TriggeredScene { world: { buffers, waveshaperArray, periodic } }) = do
  ichange' (Proxy :: _ "mainFader") 1.0
  ichange' (Proxy :: _ "subMainFader") 1.0
  ichange' (Proxy :: _ "loopingBuffer0") 0.0
  ichange' (Proxy :: _ "loopingBuffer0Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer0PlayBuf") { buffer: buffers.loops.b0, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer1") 0.0
  ichange' (Proxy :: _ "loopingBuffer1Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer1PlayBuf") { buffer: buffers.loops.b1, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer2") 0.0
  ichange' (Proxy :: _ "loopingBuffer2Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer2PlayBuf") { buffer: buffers.loops.b2, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer3") 0.0
  ichange' (Proxy :: _ "loopingBuffer3Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer3PlayBuf") { buffer: buffers.loops.b3, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer4") 0.0
  ichange' (Proxy :: _ "loopingBuffer4Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer4PlayBuf") { buffer: buffers.loops.b4, onOff: _on }

setup0 :: forall proof. TriggeredScene Trigger World () -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
setup0 (TriggeredScene { world: { buffers, waveshaperArray, periodic } }) = do
  ichange' (Proxy :: _ "mainFader") 1.0
  ichange' (Proxy :: _ "subMainDelay") 0.3
  ichange' (Proxy :: _ "subMainGain") 0.0
  ichange' (Proxy :: _ "subMainFader") 1.0
  -- looping buffers
  ichange' (Proxy :: _ "loopingBuffer0") 0.0
  ichange' (Proxy :: _ "loopingBuffer0Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer0PlayBuf") { buffer: buffers.loops.b0, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer1") 0.0
  ichange' (Proxy :: _ "loopingBuffer1Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer1PlayBuf") { buffer: buffers.loops.b1, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer2") 0.0
  ichange' (Proxy :: _ "loopingBuffer2Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer2PlayBuf") { buffer: buffers.loops.b2, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer3") 0.0
  ichange' (Proxy :: _ "loopingBuffer3Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer3PlayBuf") { buffer: buffers.loops.b3, onOff: _on }
  ichange' (Proxy :: _ "loopingBuffer4") 0.0
  ichange' (Proxy :: _ "loopingBuffer4Pan") 0.0
  ichange' (Proxy :: _ "loopingBuffer4PlayBuf") { buffer: buffers.loops.b4, onOff: _on }
  -- bells
  ichange' (Proxy :: _ "distantBells") 0.0
  ichange' (Proxy :: _ "distantBellsPan") 0.0
  ichange' (Proxy :: _ "distantBellsPlayBuf")
    { buffer: buffers.bells
    , onOff: _on
    }
  -- uncontrollable singleton
  ichange' (Proxy :: _ "uSingleton") 0.0
  ichange' (Proxy :: _ "uSingletonGain") 0.0
  ichange' (Proxy :: _ "uSingletonSource") 1.0
  ichange' (Proxy :: _ "uSingletonPlayBuf0") buffers.uncontrollable
  ichange' (Proxy :: _ "uSingletonPlayBuf1") buffers.uncontrollable
  ichange' (Proxy :: _ "uSingletonPlayBuf2") buffers.uncontrollable
  ichange' (Proxy :: _ "uSingletonPlayBuf3") buffers.uncontrollable
  ichange' (Proxy :: _ "uSingletonPlayBuf4") buffers.uncontrollable
  ichange' (Proxy :: _ "uSingletonHGain") 0.0
  ichange' (Proxy :: _ "uSingletonHHPF") 2500.0
  ichange' (Proxy :: _ "uSingletonHDelay") 0.6
  -- pad
  ichange' (Proxy :: _ "pad") 0.0
  ichange' (Proxy :: _ "padDry") 1.0
  ichange' (Proxy :: _ "padWaveshaped") 0.0
  ichange' (Proxy :: _ "padWaveshaper") waveshaperArray -- waveshape
  ichange' (Proxy :: _ "padMix") 1.0
  ichange' (Proxy :: _ "padFilter0") { freq: C.padFilter0Freq, q: C.padFilter0Q }
  ichange' (Proxy :: _ "padFilter1") { freq: C.padFilter1Freq, q: C.padFilter1Q }
  ichange' (Proxy :: _ "padFilter2") { freq: C.padFilter2Freq, q: C.padFilter2Q }
  ichange' (Proxy :: _ "padFilter3") { freq: C.padFilter3Freq, q: C.padFilter3Q }
  ichange' (Proxy :: _ "padSources") 1.0
  ichange' (Proxy :: _ "padSource0") 0.1 -- better val
  ichange' (Proxy :: _ "padSource0LFO") 1.0
  ichange' (Proxy :: _ "padSource0Pan") 0.0
  ichange' (Proxy :: _ "padSource0Osc") periodic -- periodic osc
  ichange' (Proxy :: _ "padSource1") 0.05 -- better val
  ichange' (Proxy :: _ "padSource1LFO") 1.0
  ichange' (Proxy :: _ "padSource1Pan") 0.0
  ichange' (Proxy :: _ "padSource1Osc") C.padOsc1Freq
  ichange' (Proxy :: _ "padSource2") 0.02 -- better val
  ichange' (Proxy :: _ "padSource2LFO") 1.0
  ichange' (Proxy :: _ "padSource2Pan") 0.0
  ichange' (Proxy :: _ "padSource2Osc") C.padOsc2Freq
  ichange' (Proxy :: _ "padSource3") 0.02 -- better val
  ichange' (Proxy :: _ "padSource3LFO") 1.0
  ichange' (Proxy :: _ "padSource3Pan") 0.0
  ichange' (Proxy :: _ "padSource3Osc") C.padOsc3Freq
  ichange' (Proxy :: _ "padSource4") 0.02 -- better val
  ichange' (Proxy :: _ "padSource4LFO") 1.0
  ichange' (Proxy :: _ "padSource4Pan") 0.0
  ichange' (Proxy :: _ "padSource4Osc") C.padOsc4Freq
  -- drone
  ichange' (Proxy :: _ "drone") 0.0
  ichange' (Proxy :: _ "dronePiecewiseGain") 1.0
  ichange' (Proxy :: _ "droneFilter0") { freq: 100.0, q: 0.02 } -- better val
  ichange' (Proxy :: _ "droneFilter1") { freq: 650.0, q: 0.02 } -- better val
  ichange' (Proxy :: _ "droneFilter2") { freq: 1240.0, q: 0.02 } -- better val
  ichange' (Proxy :: _ "droneFilter3") { freq: 2300.0, q: 0.02 } -- better val
  ichange' (Proxy :: _ "droneSources") 1.0
  ichange' (Proxy :: _ "droneDelayed") 0.0
  ichange' (Proxy :: _ "droneDelayLine") 0.04 -- flange
  ichange' (Proxy :: _ "droneDelayHPF") 2600.0
  ichange' (Proxy :: _ "droneProtoSources") 1.0
  ichange' (Proxy :: _ "droneSource0") 1.0
  ichange' (Proxy :: _ "droneSource0Buf") { buffer: buffers.drones.d0, onOff: _on }
  ichange' (Proxy :: _ "droneSource1") 0.0 -- is this 0'd out?
  ichange' (Proxy :: _ "droneSource1Buf") { buffer: buffers.drones.d1, onOff: _on }
  ichange' (Proxy :: _ "droneSource2") 0.0
  ichange' (Proxy :: _ "droneSource2Buf") { buffer: buffers.drones.d2, onOff: _on }
  ichange' (Proxy :: _ "droneSource3") 0.0
  ichange' (Proxy :: _ "droneSource3Buf") { buffer: buffers.drones.d3, onOff: _on }
  ichange' (Proxy :: _ "droneSource4") 0.0
  ichange' (Proxy :: _ "droneSource4Buf") { buffer: buffers.drones.d4, onOff: _on }
  -- smplr
  ichange' (Proxy :: _ "smplr") 0.0
  ichange' (Proxy :: _ "smplrMain") 1.0
  ----
  ichange' (Proxy :: _ "smplrDelay0") 1.0
  ichange' (Proxy :: _ "smplrSourcesGain0") 1.0
  ichange' (Proxy :: _ "smplrDelay0Gain") 1.0
  ichange' (Proxy :: _ "smplrHighpass0") 3000.0
  ichange' (Proxy :: _ "smplrDelayD0") 0.15
  ----
  ichange' (Proxy :: _ "smplrDelay1") 0.0
  ichange' (Proxy :: _ "smplrSourcesGain1") 1.0
  ichange' (Proxy :: _ "smplrDelay1Gain") 1.0
  ichange' (Proxy :: _ "smplrHighpass1") 2500.0
  ichange' (Proxy :: _ "smplrDelayD1") 0.2
  ----
  ichange' (Proxy :: _ "smplrDelay2") 0.0
  ichange' (Proxy :: _ "smplrSourcesGain2") 1.0
  ichange' (Proxy :: _ "smplrDelay2Gain") 1.0
  ichange' (Proxy :: _ "smplrHighpass2") 2000.0
  ichange' (Proxy :: _ "smplrDelayD2") 0.5
  ----
  ichange' (Proxy :: _ "smplrDelayCbnd") 0.0
  ichange' (Proxy :: _ "smplrSourcesGainCbnd") 1.0
  ichange' (Proxy :: _ "smplrDelayCbndGain") 1.0
  ichange' (Proxy :: _ "smplrHighpassCbnd") 1500.0
  ichange' (Proxy :: _ "smplrDelayDCbnd") 0.9
  ----
  ichange' (Proxy :: _ "smplrSources") 1.0
  ichange' (Proxy :: _ "smplrSource0") 1.0
  --, smplrSource0Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "smplrSource1") 1.0
  --, smplrSource1Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "smplrSource2") 1.0
  --, smplrSource2Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "smplrSource3") 1.0
  --, smplrSource3Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "smplrSource4") 1.0
  --, smplrSource4Buf :: TPlayBuf /\ {}
  -- lead
  ichange' (Proxy :: _ "lead") 0.0
  ichange' (Proxy :: _ "leadMain") 1.0
  ichange' (Proxy :: _ "leadDelay0") 0.0
  ichange' (Proxy :: _ "leadSourcesGain0") 1.0
  ichange' (Proxy :: _ "leadDelay0Gain") 1.0
  ichange' (Proxy :: _ "leadHighpass0") 2500.0
  ichange' (Proxy :: _ "leadDelayD0") 0.7
  ----
  ichange' (Proxy :: _ "leadDelay1") 0.0
  ichange' (Proxy :: _ "leadSourcesGain1") 1.0
  ichange' (Proxy :: _ "leadDelay1Gain") 0.0
  ichange' (Proxy :: _ "leadHighpass1") 2500.0
  ichange' (Proxy :: _ "leadDelayD1") 0.5
  ----
  ichange' (Proxy :: _ "leadDelay2") 0.0
  ichange' (Proxy :: _ "leadSourcesGain2") 1.0
  ichange' (Proxy :: _ "leadDelay2Gain") 1.0
  ichange' (Proxy :: _ "leadHighpass2") 1500.0
  ichange' (Proxy :: _ "leadDelayD2") 0.35
  ----
  ichange' (Proxy :: _ "leadDelayCbnd") 0.0
  ichange' (Proxy :: _ "leadSourcesGainCbnd") 1.0
  ichange' (Proxy :: _ "leadDelayCbndGain") 1.0
  ichange' (Proxy :: _ "leadHighpassCbnd") 1500.0
  ichange' (Proxy :: _ "leadDelayDCbnd") 0.2
  ----
  ichange' (Proxy :: _ "leadSources") 1.0
  ichange' (Proxy :: _ "leadSource0") 1.0
  --, leadSource0Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "leadSource1") 1.0
  --, leadSource1Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "leadSource2") 1.0
  --, leadSource2Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "leadSource3") 1.0
  --, leadSource3Buf :: TPlayBuf /\ {}
  ichange' (Proxy :: _ "leadSource4") 1.0
  --, leadSource4Buf :: TPlayBuf /\ {}
  --
  ichange' (Proxy :: _ "slCombo") 1.0
  ichange' (Proxy :: _ "slCombo0") 0.0
  ichange' (Proxy :: _ "slCombo0Proto") 1.0
  ichange' (Proxy :: _ "slCombo0Delay") 0.25
  ichange' (Proxy :: _ "slCombo0DelayLine") 0.2
  ichange' (Proxy :: _ "slCombo0Filter") 150.0
  ichange' (Proxy :: _ "slCombo1") 0.0
  ichange' (Proxy :: _ "slCombo1Proto") 1.0
  ichange' (Proxy :: _ "slCombo1Delay") 0.3
  ichange' (Proxy :: _ "slCombo1DelayLine") 0.37
  ichange' (Proxy :: _ "slCombo1Filter") 700.0
  ichange' (Proxy :: _ "slCombo2") 0.0
  ichange' (Proxy :: _ "slCombo2Proto") 1.0
  ichange' (Proxy :: _ "slCombo2Delay") 1.0
  ichange' (Proxy :: _ "slCombo2DelayLine") 0.45
  ichange' (Proxy :: _ "slCombo2Filter") 2500.0

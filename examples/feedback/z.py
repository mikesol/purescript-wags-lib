A='''    mainFader: 1.0
  , subMainDelay: 0.3
  , subMainGain: 0.0
  , subMainFader: 1.0
  -- looping buffers
  , loopingBuffer0: 0.0
  , loopingBuffer0Pan: 0.0
  , loopingBuffer0PlayBuf: { buffer: buffers.loops.b0, onOff: _on }
  , loopingBuffer1: 0.0
  , loopingBuffer1Pan: 0.0
  , loopingBuffer1PlayBuf: { buffer: buffers.loops.b1, onOff: _on }
  , loopingBuffer2: 0.0
  , loopingBuffer2Pan: 0.0
  , loopingBuffer2PlayBuf: { buffer: buffers.loops.b2, onOff: _on }
  , loopingBuffer3: 0.0
  , loopingBuffer3Pan: 0.0
  , loopingBuffer3PlayBuf: { buffer: buffers.loops.b3, onOff: _on }
  , loopingBuffer4: 0.0
  , loopingBuffer4Pan: 0.0
  , loopingBuffer4PlayBuf: { buffer: buffers.loops.b4, onOff: _on }
  -- bells
  , distantBells: 0.0
  , distantBellsPan: 0.0
  , distantBellsPlayBuf:
      { buffer: buffers.bells
      , onOff: _on
      }
  -- uncontrollable singleton
  , uSingleton: 0.0
  , uSingletonGain: 0.0
  , uSingletonSource: 1.0
  , uSingletonPlayBuf0: buffers.uncontrollable
  , uSingletonPlayBuf1: buffers.uncontrollable
  , uSingletonPlayBuf2: buffers.uncontrollable
  , uSingletonPlayBuf3: buffers.uncontrollable
  , uSingletonPlayBuf4: buffers.uncontrollable
  , uSingletonHGain: 0.0
  , uSingletonHHPF: 2500.0
  , uSingletonHDelay: 0.6
  -- pad
  , pad: 0.0
  , padDry: 1.0
  , padWaveshaped: 0.0
  , padWaveshaper: ?hole -- waveshape
  , padMix: 1.0
  , padFilter0: { freq: C.padFilter0Freq, q: C.padFilter0Q }
  , padFilter1: { freq: C.padFilter1Freq, q: C.padFilter1Q }
  , padFilter2: { freq: C.padFilter2Freq, q: C.padFilter2Q }
  , padFilter3: { freq: C.padFilter3Freq, q: C.padFilter3Q }
  , padSources: 1.0
  , padSource0: 0.1 -- better val
  , padSource0LFO: 1.0
  , padSource0Pan: 0.0
  , padSource0Osc: ?hole -- periodic osc
  , padSource1: 0.05 -- better val
  , padSource1LFO: 1.0
  , padSource1Pan: 0.0
  , padSource1Osc: C.padOsc1Freq
  , padSource2: 0.02 -- better val
  , padSource2LFO: 1.0
  , padSource2Pan: 0.0
  , padSource2Osc: C.padOsc2Freq
  , padSource3: 0.02 -- better val
  , padSource3LFO: 1.0
  , padSource3Pan: 0.0
  , padSource3Osc: C.padOsc3Freq
  , padSource4: 0.02 -- better val
  , padSource4LFO: 1.0
  , padSource4Pan: 0.0
  , padSource4Osc: C.padOsc4Freq
  -- drone
  , drone: 0.0
  , dronePiecewiseGain: 1.0
  , droneFilter0: { freq: 100.0, q: 0.02 } -- better val
  , droneFilter1: { freq: 650.0, q: 0.02 } -- better val
  , droneFilter2: { freq: 1240.0, q: 0.02 } -- better val
  , droneFilter3: { freq: 2300.0, q: 0.02 } -- better val
  , droneSources: 1.0
  , droneDelayed: 0.0
  , droneDelayLine: 0.04 -- flange
  , droneDelayHPF: 2600.0
  , droneProtoSources: 1.0
  , droneSource0: 1.0
  , droneSource0Buf: { buffer: buffers.drones.d0, onOff: _on }
  , droneSource1: 0.0 -- is this 0'd out?
  , droneSource1Buf: { buffer: buffers.drones.d1, onOff: _on }
  , droneSource2: 0.0
  , droneSource2Buf: { buffer: buffers.drones.d2, onOff: _on }
  , droneSource3: 0.0
  , droneSource3Buf: { buffer: buffers.drones.d3, onOff: _on }
  , droneSource4: 0.0
  , droneSource4Buf: { buffer: buffers.drones.d4, onOff: _on }
  -- smplr
  , smplr: 0.0
  , smplrMain: 1.0
  ----
  , smplrDelay0: 1.0
  , smplrSourcesGain0: 1.0
  , smplrDelay0Gain: 1.0
  , smplrHighpass0: 3000.0
  , smplrDelayD0: 0.15
  ----
  , smplrDelay1: 0.0
  , smplrSourcesGain1: 1.0
  , smplrDelay1Gain: 1.0
  , smplrHighpass1: 2500.0
  , smplrDelayD1: 0.2
  ----
  , smplrDelay2: 0.0
  , smplrSourcesGain2: 1.0
  , smplrDelay2Gain: 1.0
  , smplrHighpass2: 2000.0
  , smplrDelayD2: 0.5
  ----
  , smplrDelayCbnd: 0.0
  , smplrSourcesGainCbnd: 1.0
  , smplrDelayCbndGain: 1.0
  , smplrHighpassCbnd: 1500.0
  , smplrDelayDCbnd: 0.9
  ----
  , smplrSources: 1.0
  , smplrSource0: 1.0
  --, smplrSource0Buf :: TPlayBuf /\ {}
  , smplrSource1: 1.0
  --, smplrSource1Buf :: TPlayBuf /\ {}
  , smplrSource2: 1.0
  --, smplrSource2Buf :: TPlayBuf /\ {}
  , smplrSource3: 1.0
  --, smplrSource3Buf :: TPlayBuf /\ {}
  , smplrSource4: 1.0
  --, smplrSource4Buf :: TPlayBuf /\ {}
  -- lead
  , lead: 0.0
  , leadMain: 1.0
  , leadDelay0: 0.0
  , leadSourcesGain0: 1.0
  , leadDelay0Gain: 1.0
  , leadHighpass0: 2500.0
  , leadDelayD0: 0.7
  ----
  , leadDelay1: 0.0
  , leadSourcesGain1: 1.0
  , leadDelay1Gain: 0.0
  , leadHighpass1: 2500.0
  , leadDelayD1: 0.5
  ----
  , leadDelay2: 0.0
  , leadSourcesGain2: 1.0
  , leadDelay2Gain: 1.0
  , leadHighpass2: 1500.0
  , leadDelayD2: 0.35
  ----
  , leadDelayCbnd: 0.0
  , leadSourcesGainCbnd: 1.0
  , leadDelayCbndGain: 1.0
  , leadHighpassCbnd: 1500.0
  , leadDelayDCbnd: 0.2
  ----
  , leadSources: 1.0
  , leadSource0: 1.0
  --, leadSource0Buf :: TPlayBuf /\ {}
  , leadSource1: 1.0
  --, leadSource1Buf :: TPlayBuf /\ {}
  , leadSource2: 1.0
  --, leadSource2Buf :: TPlayBuf /\ {}
  , leadSource3: 1.0
  --, leadSource3Buf :: TPlayBuf /\ {}
  , leadSource4: 1.0
  --, leadSource4Buf :: TPlayBuf /\ {}
  --
  , slCombo: 1.0
  , slCombo0: 0.0
  , slCombo0Proto: 1.0
  , slCombo0Delay: 0.25
  , slCombo0DelayLine: 0.2
  , slCombo0Filter: 150.0
  , slCombo1: 0.0
  , slCombo1Proto: 1.0
  , slCombo1Delay: 0.3
  , slCombo1DelayLine: 0.37
  , slCombo1Filter: 700.0
  , slCombo2: 0.0
  , slCombo2Proto: 1.0
  , slCombo2Delay: 1.0
  , slCombo2DelayLine: 0.45
  , slCombo2Filter: 2500.0
  '''.split('\n')

for x in A:
    if ' , ' in x:
        print(x.replace(':', "\")", 1).replace(', ', "ichange' (Proxy :: _ \"", 1))
    else: print(x)
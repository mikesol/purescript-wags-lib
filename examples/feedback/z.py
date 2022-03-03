from multiprocessing.sharedctypes import Value


A='''    { -- press to grow or shrink a pad
    triggerPad: Pad (Rect 60 60 240 240) focusc 0.0
  -- periodic wave in mix
  , togglePadOsc0: T2 (Rect 400 60 60 60) focusc T2_0
  -- triangle wave in mix
  , togglePadOsc1: T2 (Rect 540 940 60 60) focusc T2_0
  -- sawtooth wave in mix
  , togglePadOsc2: T2 (Rect 270 690 60 60) focusc T2_0
  -- square in mix
  , togglePadOsc3: T2 (Rect 0 60 60 60) focusc T2_0
  -- high sine in mix
  , togglePadOsc4: T2 (Rect 420 940 60 60) focusc T2_0
  -- three detuning factors + "normal" harmonic series
  , detunePad: T4 (Rect 800 690 130 100) focusc T4_0
  -- 0th lfo on the pad gain
  , gainLFO0Pad: Slider GainLFO0Pad _.interactions.gainLFO0Pad (Rect 90 810 450 60) focusc 0.0
  -- 1st lfo on the pad gain, fatter & more sluggish
  , gainLFO1Pad: Slider GainLFO1Pad _.interactions.gainLFO1Pad (Rect 630 690 90 180) focusc 0.0
  -- pad filter bank chooser
  , filterBankChooserPad: T5 (Rect 400 180 120 120) focusc T5_0 --
  -- waveshaper on the pad
  , waveshaperPad: Slider WaveshaperPad _.interactions.waveshaperPad (Rect 800 0 200 70) focusc 0.0
  -- trigger synth
  , triggerLead: Source (Rect 720 790 210 210) focusc
  -- the synth we use for the lead
  , synthForLead: T6 (Rect 590 460 340 130) focusc T6_0
  -- choose which pitch out of 14 to start at
  , pitchLead: DiscreteChooser PitchLead _.interactions.pitchLead (Rect 930 70 70 520) focusc 14 0
  -- 0th delay line for the lead synth
  , leadDelayLine0: T2 (Rect 0 240 60 60) focusc T2_0
  -- 1st delay line for the lead synth
  , leadDelayLine1: T2 (Rect 460 120 60 60) focusc T2_0
  -- 2nd delay line for the lead synth
  , leadDelayLine2: T2 (Rect 90 690 60 60) focusc T2_0
  -- combined delay line for the lead synth
  , leadCombinedDelay0: T2 (Rect 300 0 60 60) focusc T2_0
  -- shifts intensities of delays
  , leadDelayGainCarousel: Slider LeadDelayGainCarousel _.interactions.leadDelayGainCarousel (Rect 590 590 410 100) focusc 0.0
  -- n pitches played when pressed (fixed sequence always based on start)
  , nPitchesPlayedLead: DiscreteChooser NPitchesPlayedLead _.interactions.nPitchesPlayedLead (Rect 90 300 90 390) focusc 7 0
  -- one of three envelopes
  , envelopeLead: T3 (Rect 660 0 140 140) focusc T3_0
  -- octave shift
  , octaveLead: T3 (Rect 800 70 60 70) focusc T3_0
  -- pad for a buffer
  , drone: Pad (Rect 270 390 320 300) focusc 0.0
  -- choose drone
  , droneChooser: T5 (Rect 390 690 150 120) focusc T5_0
  -- lowpass q for drone
  , droneLowpass0Q: Slider DroneLowpass0Q _.interactions.droneLowpass0Q (Rect 540 690 90 180) focusc 0.0
  -- q of bandpass filter
  , droneBandpass0Q: Slider DroneBandpass0Q _.interactions.droneBandpass0Q (Rect 0 940 300 60) focusc 0.0
  -- lfo controlling freq of bandpass
  , droneBandpass0LFO: Slider DroneBandpass0LFO _.interactions.droneBandpass0LFO (Rect 520 60 70 240) focusc 0.0
  -- q of bandpass filter
  , droneBandpass1Q: Slider DroneBandpass1Q _.interactions.droneBandpass1Q (Rect 930 690 70 310) focusc 0.0
  -- lfo of bandpass filter
  , droneBandpass1LFO: Slider DroneBandpass1LFO _.interactions.droneBandpass1LFO (Rect 180 300 90 260) focusc 0.0
  -- q of highpass filter
  , droneHighpass0Q: Slider DroneHighpass0Q _.interactions.droneHighpass0Q (Rect 860 70 70 200) focusc 0.0
  -- lfo of highpass filter
  , droneHighpass0LFO: Slider DroneHighpass0LFO _.interactions.droneHighpass0LFO (Rect 360 0 300 60) focusc 0.0
  -- how long does it take for the drone to ramp up?
  , droneActivationEnergyThreshold: Slider DroneActivationEnergyThreshold _.interactions.droneActivationEnergyThreshold (Rect 90 750 300 60) focusc 0.0
  -- looping pwf
  , droneRhythmicLoopingPiecewiseFunction: T5 (Rect 660 340 270 120) focusc T5_0
  -- how long does the drone linger?
  , droneDecay: Slider DroneDecay _.interactions.droneDecay (Rect 590 60 70 400) focusc 0.0
  -- flange on the drone
  , droneFlange: T2 (Rect 60 0 60 60) focusc T2_0
  -- a single sample
  , sampleOneShot: Source (Rect 660 140 200 200) focusc
  -- reverse the samples?
  , sampleReverse: T2 (Rect 400 120 60 60) focusc T2_0
  -- choose which sample to play
  , sampleChooser: DiscreteChooser SampleChooser _.interactions.sampleChooser (Rect 0 300 90 640) focusc 24 0
  , sampleChorusEffect: T2 (Rect 120 0 60 60) focusc T2_0
  , sampleRateChange: T3 (Rect 720 690 80 100) focusc T3_0
  -- 0th delay line for the single sample
  , sampleDelayLine0: T2 (Rect 0 0 60 60) focusc T2_0
  -- 1st delay line for the single sample
  , sampleDelayLine1: T2 (Rect 150 690 60 60) focusc T2_0
  -- 2nd delay line for the single sample
  , sampleDelayLine2: T2 (Rect 860 270 70 70) focusc T2_0
  -- 0th sample combined delay line
  , sampleCombinedDelayLine0: T2 (Rect 480 940 60 60) focusc T2_0
  -- changes intensities of various delay lines
  , sampleDelayGainCarousel: Slider SampleDelayGainCarousel _.interactions.sampleDelayGainCarousel (Rect 270 300 320 90) focusc 0.0
  -- 0th delay line for combined lead sample
  , leadSampleDelayLine0: T2 (Rect 660 940 60 60) focusc T2_0
  -- 1st delay line for combined lead sample
  , leadSampleDelayLine1: T2 (Rect 330 690 60 60) focusc T2_0
  -- 2nd delay line for combined lead sample
  , leadSampleDelayLine2: T2 (Rect 0 180 60 60) focusc T2_0
  -- alternates between any looping buffers that are currently playing
  , loopingBufferGainDJ: T2 (Rect 240 0 60 60) focusc T2_0
  -- how close the start/end times are
  , loopingBufferStartEndConstriction: Slider LoopingBufferStartEndConstriction _.interactions.loopingBufferStartEndConstriction (Rect 300 160 100 140) focusc 0.0
  -- 0th looping buffer
  , loopingBuffer0: T2 (Rect 180 0 60 60) focusc T2_0
  -- 1st looping buffer
  , loopingBuffer1: T2 (Rect 210 690 60 60) focusc T2_0
  -- 2nd looping buffer
  , loopingBuffer2: T2 (Rect 360 940 60 60) focusc T2_0
  -- 3rd looping buffer
  , loopingBuffer3: T2 (Rect 300 940 60 60) focusc T2_0
  -- 4th looping buffer
  , loopingBuffer4: T2 (Rect 0 120 60 60) focusc T2_0
  -- substitutes entirely different sets of base parameters
  , radicalFlip: T2 (Rect 460 60 60 60) focusc T2_0
  -- global pan extravaganza
  , greatAndMightyPan: Slider GreatAndMightyPan _.interactions.greatAndMightyPan (Rect 90 870 630 70) focusc 0.0
  -- global delay
  , globalDelay: T2 (Rect 600 940 60 60) focusc T2_0
  -- echoing uncontrollable singleton
  , echoingUncontrollableSingleton: Source (Rect 300 60 100 100) focusc
  , distantBellsFader: Slider DistantBellsFader _.interactions.distantBellsFader (Rect 180 560 90 130) focusc 0.0
  }'''

A = A.split('\n')
for i, x in enumerate(A):
  y = x.split(' ')
  for token in ['Slider', 'DiscreteChooser']:
    if token in y:
      if 'focusc' not in y : raise ValueError('focusc not found in ' + x)
      ix = y.index('focusc')
      y = y[:ix] + ['backgroundc','foregroundc'] + y[ix+1:]
  A[i] = ' '.join(y)

print('\n'.join(A))

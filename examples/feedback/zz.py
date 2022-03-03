A='''  | GainLFO0Pad SliderAction
  | GainLFO1Pad SliderAction
  | WaveshaperPad SliderAction
  | PitchLead SliderAction
  | LeadDelayGainCarousel SliderAction
  | NPitchesPlayedLead SliderAction
  | DroneLowpass0Q SliderAction
  | DroneBandpass0Q SliderAction
  | DroneBandpass0LFO SliderAction
  | DroneBandpass1Q SliderAction
  | DroneBandpass1LFO SliderAction
  | DroneHighpass0Q SliderAction
  | DroneHighpass0LFO SliderAction
  | DroneActivationEnergyThreshold SliderAction
  | DroneDecay SliderAction
  | SampleChooser SliderAction
  | SampleDelayGainCarousel SliderAction
  | LoopingBufferStartEndConstriction SliderAction
  | GreatAndMightyPan SliderAction
  | DistantBellsFader SliderAction'''

A=[[y for y in x.split(' ') if y != '' and y != '|'][0] for x in A.split('\n')]
for x in A:
  print("", ",",x[0].lower()+x[1:],": uzto >>> SliderRemoteMove >>>", x, ">>> HS.notify listener")
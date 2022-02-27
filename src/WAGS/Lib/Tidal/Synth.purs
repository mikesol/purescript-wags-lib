module WAGS.Lib.Tidal.Synth
  ( Full
  , Opt
  , m2f
  , midify
  , periodicSynth
  , periodicSynth_
  , pitches
  , sawtoothSynth
  , sawtoothSynth_
  , sineSynth
  , sineSynth_
  , squareSynth
  , squareSynth_
  , synth
  , triangleSynth
  , triangleSynth_
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, D1)
import Data.Variant.Either (either)
import Data.Vec (Vec)
import Foreign.Object (fromHomogeneous)
import Foreign.Object as Object
import Math (pow)
import Prim.Row (class Nub, class Union)
import Record as Record
import WAGS.Create.Optionals (gain, periodicOsc, sawtoothOsc, sinOsc, squareOsc, triangleOsc)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Piecewise (APFofT, makePiecewise)
import WAGS.Lib.Tidal.FX (goodbye, hello, fx)
import WAGS.Lib.Tidal.Types (Note(..), Sample(..), TimeIs(..), UnsampledTimeIs(..))
import WAGS.Tumult (Tumultuous)

pitches :: Object.Object Number
pitches = fromHomogeneous
  { cflat0: 11.0
  , cflat1: 23.0
  , cflat2: 35.0
  , cflat3: 47.0
  , cflat4: 59.0
  , cflat5: 71.0
  , cflat6: 83.0
  , cflat7: 95.0
  , cflat8: 107.0
  , cflat9: 119.0
  , cflat10: 131.0
  , cflat11: 143.0
  , cflat12: 155.0
  , cflat13: 167.0
  , c0: 12.0
  , c1: 24.0
  , c2: 36.0
  , c3: 48.0
  , c4: 60.0
  , c5: 72.0
  , c6: 84.0
  , c7: 96.0
  , c8: 108.0
  , c9: 120.0
  , c10: 132.0
  , c11: 144.0
  , c12: 156.0
  , c13: 168.0
  , csharp0: 13.0
  , csharp1: 25.0
  , csharp2: 37.0
  , csharp3: 49.0
  , csharp4: 61.0
  , csharp5: 73.0
  , csharp6: 85.0
  , csharp7: 97.0
  , csharp8: 109.0
  , csharp9: 121.0
  , csharp10: 133.0
  , csharp11: 145.0
  , csharp12: 157.0
  , csharp13: 169.0
  , dflat0: 13.0
  , dflat1: 25.0
  , dflat2: 37.0
  , dflat3: 49.0
  , dflat4: 61.0
  , dflat5: 73.0
  , dflat6: 85.0
  , dflat7: 97.0
  , dflat8: 109.0
  , dflat9: 121.0
  , dflat10: 133.0
  , dflat11: 145.0
  , dflat12: 157.0
  , dflat13: 169.0
  , d0: 14.0
  , d1: 26.0
  , d2: 38.0
  , d3: 50.0
  , d4: 62.0
  , d5: 74.0
  , d6: 86.0
  , d7: 98.0
  , d8: 110.0
  , d9: 122.0
  , d10: 134.0
  , d11: 146.0
  , d12: 158.0
  , d13: 170.0
  , dsharp0: 15.0
  , dsharp1: 27.0
  , dsharp2: 39.0
  , dsharp3: 51.0
  , dsharp4: 63.0
  , dsharp5: 75.0
  , dsharp6: 87.0
  , dsharp7: 99.0
  , dsharp8: 111.0
  , dsharp9: 123.0
  , dsharp10: 135.0
  , dsharp11: 147.0
  , dsharp12: 159.0
  , dsharp13: 171.0
  , eflat0: 15.0
  , eflat1: 27.0
  , eflat2: 39.0
  , eflat3: 51.0
  , eflat4: 63.0
  , eflat5: 75.0
  , eflat6: 87.0
  , eflat7: 99.0
  , eflat8: 111.0
  , eflat9: 123.0
  , eflat10: 135.0
  , eflat11: 147.0
  , eflat12: 159.0
  , eflat13: 171.0
  , e0: 16.0
  , e1: 28.0
  , e2: 40.0
  , e3: 52.0
  , e4: 64.0
  , e5: 76.0
  , e6: 88.0
  , e7: 100.0
  , e8: 112.0
  , e9: 124.0
  , e10: 136.0
  , e11: 148.0
  , e12: 160.0
  , e13: 172.0
  , esharp0: 17.0
  , esharp1: 29.0
  , esharp2: 41.0
  , esharp3: 53.0
  , esharp4: 65.0
  , esharp5: 77.0
  , esharp6: 89.0
  , esharp7: 101.0
  , esharp8: 113.0
  , esharp9: 125.0
  , esharp10: 137.0
  , esharp11: 149.0
  , esharp12: 161.0
  , esharp13: 173.0
  , fflat0: 16.0
  , fflat1: 28.0
  , fflat2: 40.0
  , fflat3: 52.0
  , fflat4: 64.0
  , fflat5: 76.0
  , fflat6: 88.0
  , fflat7: 100.0
  , fflat8: 112.0
  , fflat9: 124.0
  , fflat10: 136.0
  , fflat11: 148.0
  , fflat12: 160.0
  , fflat13: 172.0
  , f0: 17.0
  , f1: 29.0
  , f2: 41.0
  , f3: 53.0
  , f4: 65.0
  , f5: 77.0
  , f6: 89.0
  , f7: 101.0
  , f8: 113.0
  , f9: 125.0
  , f10: 137.0
  , f11: 149.0
  , f12: 161.0
  , f13: 173.0
  , fsharp0: 18.0
  , fsharp1: 30.0
  , fsharp2: 42.0
  , fsharp3: 54.0
  , fsharp4: 66.0
  , fsharp5: 78.0
  , fsharp6: 90.0
  , fsharp7: 102.0
  , fsharp8: 114.0
  , fsharp9: 126.0
  , fsharp10: 138.0
  , fsharp11: 150.0
  , fsharp12: 162.0
  , fsharp13: 174.0
  , gflat0: 18.0
  , gflat1: 30.0
  , gflat2: 42.0
  , gflat3: 54.0
  , gflat4: 66.0
  , gflat5: 78.0
  , gflat6: 90.0
  , gflat7: 102.0
  , gflat8: 114.0
  , gflat9: 126.0
  , gflat10: 138.0
  , gflat11: 150.0
  , gflat12: 162.0
  , gflat13: 174.0
  , g0: 19.0
  , g1: 31.0
  , g2: 43.0
  , g3: 55.0
  , g4: 67.0
  , g5: 79.0
  , g6: 91.0
  , g7: 103.0
  , g8: 115.0
  , g9: 127.0
  , g10: 139.0
  , g11: 151.0
  , g12: 163.0
  , g13: 175.0
  , gsharp0: 20.0
  , gsharp1: 32.0
  , gsharp2: 44.0
  , gsharp3: 56.0
  , gsharp4: 68.0
  , gsharp5: 80.0
  , gsharp6: 92.0
  , gsharp7: 104.0
  , gsharp8: 116.0
  , gsharp9: 128.0
  , gsharp10: 140.0
  , gsharp11: 152.0
  , gsharp12: 164.0
  , gsharp13: 176.0
  , aflat0: 20.0
  , aflat1: 32.0
  , aflat2: 44.0
  , aflat3: 56.0
  , aflat4: 68.0
  , aflat5: 80.0
  , aflat6: 92.0
  , aflat7: 104.0
  , aflat8: 116.0
  , aflat9: 128.0
  , aflat10: 140.0
  , aflat11: 152.0
  , aflat12: 164.0
  , aflat13: 176.0
  , a0: 21.0
  , a1: 33.0
  , a2: 45.0
  , a3: 57.0
  , a4: 69.0
  , a5: 81.0
  , a6: 93.0
  , a7: 105.0
  , a8: 117.0
  , a9: 129.0
  , a10: 141.0
  , a11: 153.0
  , a12: 165.0
  , a13: 177.0
  , asharp0: 22.0
  , asharp1: 34.0
  , asharp2: 46.0
  , asharp3: 58.0
  , asharp4: 70.0
  , asharp5: 82.0
  , asharp6: 94.0
  , asharp7: 106.0
  , asharp8: 118.0
  , asharp9: 130.0
  , asharp10: 142.0
  , asharp11: 154.0
  , asharp12: 166.0
  , asharp13: 178.0
  , bflat0: 22.0
  , bflat1: 34.0
  , bflat2: 46.0
  , bflat3: 58.0
  , bflat4: 70.0
  , bflat5: 82.0
  , bflat6: 94.0
  , bflat7: 106.0
  , bflat8: 118.0
  , bflat9: 130.0
  , bflat10: 142.0
  , bflat11: 154.0
  , bflat12: 166.0
  , bflat13: 178.0
  , b0: 23.0
  , b1: 35.0
  , b2: 47.0
  , b3: 59.0
  , b4: 71.0
  , b5: 83.0
  , b6: 95.0
  , b7: 107.0
  , b8: 119.0
  , b9: 131.0
  , b10: 143.0
  , b11: 155.0
  , b12: 167.0
  , b13: 179.0
  , bsharp0: 24.0
  , bsharp1: 36.0
  , bsharp2: 48.0
  , bsharp3: 60.0
  , bsharp4: 72.0
  , bsharp5: 84.0
  , bsharp6: 96.0
  , bsharp7: 108.0
  , bsharp8: 120.0
  , bsharp9: 132.0
  , bsharp10: 144.0
  , bsharp11: 156.0
  , bsharp12: 168.0
  , bsharp13: 180.0
  }

midify :: Sample -> Number
midify (Sample ss) = fromMaybe a4 (Object.lookup ss pitches)

defaultPw =
  makePiecewise
    ( (0.0 /\ 0.0)
        :| (0.15 /\ 0.6)
          : (0.25 /\ 0.1)
          : (0.45 /\ 0.0)
          : Nil
    ) :: APFofT

triangleSynth ∷ Note ~> Note
triangleSynth = triangleSynth_ defaultPw

triangleSynth_ :: APFofT -> Note ~> Note
triangleSynth_ pw = synth
  { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
      $ goodbye
      $ gain
          (ff 0.03 $ pw { time: sampleTime, headroomInSeconds })
          { ipt: hello
          , osc: triangleOsc ifo.freq
          }
  }

squareSynth ∷ Note ~> Note
squareSynth = squareSynth_ defaultPw

squareSynth_ :: APFofT -> Note ~> Note
squareSynth_ pw = synth
  { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
      $ goodbye
      $ gain
          (ff 0.03 $ pw { time: sampleTime, headroomInSeconds })
          { ipt: hello
          , osc: squareOsc ifo.freq
          }
  }

sawtoothSynth ∷ Note ~> Note
sawtoothSynth = sawtoothSynth_ defaultPw

sawtoothSynth_ :: APFofT -> Note ~> Note
sawtoothSynth_ pw = synth
  { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
      $ goodbye
      $ gain
          (ff 0.03 $ pw { time: sampleTime, headroomInSeconds })
          { ipt: hello
          , osc: sawtoothOsc ifo.freq
          }
  }

sineSynth ∷ Note ~> Note
sineSynth = sineSynth_ defaultPw

sineSynth_ :: APFofT -> Note ~> Note
sineSynth_ pw = synth
  { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
      $ goodbye
      $ gain
          (ff 0.03 $ pw { time: sampleTime, headroomInSeconds })
          { ipt: hello
          , osc: sinOsc ifo.freq
          }
  }

periodicSynth ∷ ∀ size. Lt D1 size ⇒ (Vec size Number /\ Vec size Number) → Note ~> Note
periodicSynth = periodicSynth_ defaultPw

periodicSynth_ :: forall size. Lt D1 size => APFofT -> (Vec size Number /\ Vec size Number) -> Note ~> Note
periodicSynth_ pw v = synth
  { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
      $ goodbye
      $ gain
          (ff 0.03 $ pw { time: sampleTime, headroomInSeconds })
          { ipt: hello
          , osc: periodicOsc v ifo.freq
          }
  }

synth
  :: forall inRec overfull event
   . Union inRec (Opt event) overfull
  => Nub overfull (Full event)
  => { | inRec }
  -> Note event
  -> Note event
synth r (Note n@{ sampleFoT }) = Note
  ( n
      { tumultFoT = \ti@(TimeIs i) ->
          let
            sample = either
              ( \f -> f
                  ( UnsampledTimeIs
                      { event: i.event
                      , clockTime: i.clockTime
                      , headroomInSeconds: i.headroomInSeconds
                      , bigCycleTime: i.bigCycleTime
                      , externalControl: i.externalControl
                      , littleCycleTime: i.littleCycleTime
                      , normalizedClockTime: i.normalizedClockTime
                      , normalizedBigCycleTime: i.normalizedBigCycleTime
                      , normalizedLittleCycleTime: i.normalizedLittleCycleTime
                      , littleCycleDuration: i.littleCycleDuration
                      , bigCycleDuration: i.bigCycleDuration
                      , entropy: i.entropy
                      , initialEntropy: i.initialEntropy
                      }
                  )
              )
              identity
              sampleFoT
            { midi, freq } = xtra sample
          in
            full.synth { midi, freq, sample } ti
      }
  )
  where
  full :: { | Full event }
  full = Record.merge r
    ( ( { synth: \ifo (TimeIs { sampleTime, headroomInSeconds }) -> fx
            $ goodbye
            $ gain
                (ff 0.03 $ defaultPw { time: sampleTime, headroomInSeconds })
                { ipt: hello
                , osc: sinOsc ifo.freq
                }
        }
      ) :: { | Full event }
    )
  xtra =
    let
      xf = \s ->
        let
          midi = midify s
        in
          { midi, freq: m2f midi }
    in
      either (const xf) (\s -> let o = xf s in const o) sampleFoT

a4 = 69.0 :: Number

m2f :: Number -> Number
m2f midi = 440.0 * (2.0 `pow` ((midi - a4) / 12.0))

type Full event = (| Opt event)
type Opt event =
  ( synth :: { sample :: Sample, freq :: Number, midi :: Number } -> TimeIs event -> Tumultuous D1 "output" (voice :: Unit)
  )

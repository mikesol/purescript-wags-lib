module WAGS.Lib.Learn.Pitch where

import Prelude

import Data.Int (round, toNumber)
import Data.Newtype (class Newtype)
import Math (log, pow)
import Safe.Coerce (coerce)

newtype Pitch = Pitch Number

derive instance newtypePitch :: Newtype Pitch _
derive newtype instance eqPitch :: Eq Pitch
derive newtype instance ordPitch :: Ord Pitch
derive newtype instance boundedPitch :: Bounded Pitch
derive newtype instance showPitch :: Show Pitch
instance commutativeRingPitch :: CommutativeRing Pitch
instance euclideanRingPitch :: EuclideanRing Pitch where
  degree (Pitch a) = degree (midiToCps' a)
  div (Pitch a) (Pitch b) = Pitch $ midiToCps' (cpsToMidi' a `div` cpsToMidi' b)
  mod (Pitch a) (Pitch b) = Pitch $ midiToCps' (cpsToMidi' a `mod` cpsToMidi' b)
instance ringPitch :: Ring Pitch where
  sub (Pitch a) (Pitch b) = Pitch $ midiToCps' (cpsToMidi' a - cpsToMidi' b)
instance semiringPitch :: Semiring Pitch where
  zero = Pitch zero
  one = Pitch one
  add (Pitch a) (Pitch b) = Pitch $ midiToCps' (cpsToMidi' a + cpsToMidi' b)
  mul (Pitch a) (Pitch b) = Pitch $ midiToCps' (cpsToMidi' a * cpsToMidi' b)

midiToPitch :: Int -> Pitch
midiToPitch = coerce <<< midiToCps

midiToCps :: Int -> Number
midiToCps = midiToCps' <<< toNumber

midiToCps' :: Number -> Number
midiToCps' i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

cpsToMidi' :: Number -> Number
cpsToMidi' i = (log (i / 440.0) / log 2.0) * 12.0 + 69.0

cpsToMidi :: Number -> Int
cpsToMidi = round <<< cpsToMidi'

pitchToMidi :: Pitch -> Int
pitchToMidi = cpsToMidi <<< coerce

semitone = Pitch (midiToCps' 1.0) :: Pitch
wholeTone = Pitch (midiToCps' 2.0) :: Pitch
minorSecond = Pitch (midiToCps' 1.0) :: Pitch
majorSecond = Pitch (midiToCps' 2.0) :: Pitch
minorThird = Pitch (midiToCps' 3.0) :: Pitch
majorThird = Pitch (midiToCps' 4.0) :: Pitch
fourth = Pitch (midiToCps' 5.0) :: Pitch
perfectFourth = Pitch (midiToCps' 5.0) :: Pitch
augmentedFourth = Pitch (midiToCps' 6.0) :: Pitch
diminishedFifth = Pitch (midiToCps' 6.0) :: Pitch
fifth = Pitch (midiToCps' 7.0) :: Pitch
perfectFifth = Pitch (midiToCps' 7.0) :: Pitch
minorSixth = Pitch (midiToCps' 8.0) :: Pitch
majorSixth = Pitch (midiToCps' 9.0) :: Pitch
minorSeventh = Pitch (midiToCps' 10.0) :: Pitch
majorSeventh = Pitch (midiToCps' 11.0) :: Pitch
octave = Pitch (midiToCps' 12.0) :: Pitch
perfectOctave = Pitch (midiToCps' 12.0) :: Pitch

c0 = Pitch (midiToCps' 12.0) :: Pitch
cSharp0 = Pitch (midiToCps' 13.0) :: Pitch
d0 = Pitch (midiToCps' 14.0) :: Pitch
dSharp0 = Pitch (midiToCps' 15.0) :: Pitch
e0 = Pitch (midiToCps' 16.0) :: Pitch
f0 = Pitch (midiToCps' 17.0) :: Pitch
fSharp0 = Pitch (midiToCps' 18.0) :: Pitch
g0 = Pitch (midiToCps' 19.0) :: Pitch
gSharp0 = Pitch (midiToCps' 20.0) :: Pitch
a0 = Pitch (midiToCps' 21.0) :: Pitch
aSharp0 = Pitch (midiToCps' 22.0) :: Pitch
b0 = Pitch (midiToCps' 23.0) :: Pitch
c1 = Pitch (midiToCps' 24.0) :: Pitch
cSharp1 = Pitch (midiToCps' 25.0) :: Pitch
d1 = Pitch (midiToCps' 26.0) :: Pitch
dSharp1 = Pitch (midiToCps' 27.0) :: Pitch
e1 = Pitch (midiToCps' 28.0) :: Pitch
f1 = Pitch (midiToCps' 29.0) :: Pitch
fSharp1 = Pitch (midiToCps' 30.0) :: Pitch
g1 = Pitch (midiToCps' 31.0) :: Pitch
gSharp1 = Pitch (midiToCps' 32.0) :: Pitch
a1 = Pitch (midiToCps' 33.0) :: Pitch
aSharp1 = Pitch (midiToCps' 34.0) :: Pitch
b1 = Pitch (midiToCps' 35.0) :: Pitch
c2 = Pitch (midiToCps' 36.0) :: Pitch
cSharp2 = Pitch (midiToCps' 37.0) :: Pitch
d2 = Pitch (midiToCps' 38.0) :: Pitch
dSharp2 = Pitch (midiToCps' 39.0) :: Pitch
e2 = Pitch (midiToCps' 40.0) :: Pitch
f2 = Pitch (midiToCps' 41.0) :: Pitch
fSharp2 = Pitch (midiToCps' 42.0) :: Pitch
g2 = Pitch (midiToCps' 43.0) :: Pitch
gSharp2 = Pitch (midiToCps' 44.0) :: Pitch
a2 = Pitch (midiToCps' 45.0) :: Pitch
aSharp2 = Pitch (midiToCps' 46.0) :: Pitch
b2 = Pitch (midiToCps' 47.0) :: Pitch
c3 = Pitch (midiToCps' 48.0) :: Pitch
cSharp3 = Pitch (midiToCps' 49.0) :: Pitch
d3 = Pitch (midiToCps' 50.0) :: Pitch
dSharp3 = Pitch (midiToCps' 51.0) :: Pitch
e3 = Pitch (midiToCps' 52.0) :: Pitch
f3 = Pitch (midiToCps' 53.0) :: Pitch
fSharp3 = Pitch (midiToCps' 54.0) :: Pitch
g3 = Pitch (midiToCps' 55.0) :: Pitch
gSharp3 = Pitch (midiToCps' 56.0) :: Pitch
a3 = Pitch (midiToCps' 57.0) :: Pitch
aSharp3 = Pitch (midiToCps' 58.0) :: Pitch
b3 = Pitch (midiToCps' 59.0) :: Pitch
c4 = Pitch (midiToCps' 60.0) :: Pitch
cSharp4 = Pitch (midiToCps' 61.0) :: Pitch
d4 = Pitch (midiToCps' 62.0) :: Pitch
dSharp4 = Pitch (midiToCps' 63.0) :: Pitch
e4 = Pitch (midiToCps' 64.0) :: Pitch
f4 = Pitch (midiToCps' 65.0) :: Pitch
fSharp4 = Pitch (midiToCps' 66.0) :: Pitch
g4 = Pitch (midiToCps' 67.0) :: Pitch
gSharp4 = Pitch (midiToCps' 68.0) :: Pitch
a4 = Pitch (midiToCps' 69.0) :: Pitch
aSharp4 = Pitch (midiToCps' 70.0) :: Pitch
b4 = Pitch (midiToCps' 71.0) :: Pitch
c5 = Pitch (midiToCps' 72.0) :: Pitch
cSharp5 = Pitch (midiToCps' 73.0) :: Pitch
d5 = Pitch (midiToCps' 74.0) :: Pitch
dSharp5 = Pitch (midiToCps' 75.0) :: Pitch
e5 = Pitch (midiToCps' 76.0) :: Pitch
f5 = Pitch (midiToCps' 77.0) :: Pitch
fSharp5 = Pitch (midiToCps' 78.0) :: Pitch
g5 = Pitch (midiToCps' 79.0) :: Pitch
gSharp5 = Pitch (midiToCps' 80.0) :: Pitch
a5 = Pitch (midiToCps' 81.0) :: Pitch
aSharp5 = Pitch (midiToCps' 82.0) :: Pitch
b5 = Pitch (midiToCps' 83.0) :: Pitch
c6 = Pitch (midiToCps' 84.0) :: Pitch
cSharp6 = Pitch (midiToCps' 85.0) :: Pitch
d6 = Pitch (midiToCps' 86.0) :: Pitch
dSharp6 = Pitch (midiToCps' 87.0) :: Pitch
e6 = Pitch (midiToCps' 88.0) :: Pitch
f6 = Pitch (midiToCps' 89.0) :: Pitch
fSharp6 = Pitch (midiToCps' 90.0) :: Pitch
g6 = Pitch (midiToCps' 91.0) :: Pitch
gSharp6 = Pitch (midiToCps' 92.0) :: Pitch
a6 = Pitch (midiToCps' 93.0) :: Pitch
aSharp6 = Pitch (midiToCps' 94.0) :: Pitch
b6 = Pitch (midiToCps' 95.0) :: Pitch
c7 = Pitch (midiToCps' 96.0) :: Pitch
cSharp7 = Pitch (midiToCps' 97.0) :: Pitch
d7 = Pitch (midiToCps' 98.0) :: Pitch
dSharp7 = Pitch (midiToCps' 99.0) :: Pitch
e7 = Pitch (midiToCps' 100.0) :: Pitch
f7 = Pitch (midiToCps' 101.0) :: Pitch
fSharp7 = Pitch (midiToCps' 102.0) :: Pitch
g7 = Pitch (midiToCps' 103.0) :: Pitch
gSharp7 = Pitch (midiToCps' 104.0) :: Pitch
a7 = Pitch (midiToCps' 105.0) :: Pitch
aSharp7 = Pitch (midiToCps' 106.0) :: Pitch
b7 = Pitch (midiToCps' 107.0) :: Pitch
dDoubleFlat0 = Pitch (midiToCps' 12.0) :: Pitch
dFlat0 = Pitch (midiToCps' 13.0) :: Pitch
cDoubleSharp0 = Pitch (midiToCps' 14.0) :: Pitch
eDoubleFlat0 = Pitch (midiToCps' 14.0) :: Pitch
eFlat0 = Pitch (midiToCps' 15.0) :: Pitch
fDoubleFlat0 = Pitch (midiToCps' 15.0) :: Pitch
dDoubleSharp0 = Pitch (midiToCps' 16.0) :: Pitch
fFlat0 = Pitch (midiToCps' 16.0) :: Pitch
eSharp0 = Pitch (midiToCps' 17.0) :: Pitch
gDoubleFlat0 = Pitch (midiToCps' 17.0) :: Pitch
eDoubleSharp0 = Pitch (midiToCps' 18.0) :: Pitch
gFlat0 = Pitch (midiToCps' 18.0) :: Pitch
fDoubleSharp0 = Pitch (midiToCps' 19.0) :: Pitch
aDoubleFlat0 = Pitch (midiToCps' 19.0) :: Pitch
aFlat0 = Pitch (midiToCps' 20.0) :: Pitch
gDoubleSharp0 = Pitch (midiToCps' 21.0) :: Pitch
bDoubleFlat0 = Pitch (midiToCps' 21.0) :: Pitch
bFlat0 = Pitch (midiToCps' 22.0) :: Pitch
cDoubleFlat1 = Pitch (midiToCps' 22.0) :: Pitch
aDoubleSharp0 = Pitch (midiToCps' 23.0) :: Pitch
cFlat1 = Pitch (midiToCps' 23.0) :: Pitch
bSharp0 = Pitch (midiToCps' 24.0) :: Pitch
dDoubleFlat1 = Pitch (midiToCps' 24.0) :: Pitch
bDoubleSharp0 = Pitch (midiToCps' 25.0) :: Pitch
dFlat1 = Pitch (midiToCps' 25.0) :: Pitch
cDoubleSharp1 = Pitch (midiToCps' 26.0) :: Pitch
eDoubleFlat1 = Pitch (midiToCps' 26.0) :: Pitch
eFlat1 = Pitch (midiToCps' 27.0) :: Pitch
fDoubleFlat1 = Pitch (midiToCps' 27.0) :: Pitch
dDoubleSharp1 = Pitch (midiToCps' 28.0) :: Pitch
fFlat1 = Pitch (midiToCps' 28.0) :: Pitch
eSharp1 = Pitch (midiToCps' 29.0) :: Pitch
gDoubleFlat1 = Pitch (midiToCps' 29.0) :: Pitch
eDoubleSharp1 = Pitch (midiToCps' 30.0) :: Pitch
gFlat1 = Pitch (midiToCps' 30.0) :: Pitch
fDoubleSharp1 = Pitch (midiToCps' 31.0) :: Pitch
aDoubleFlat1 = Pitch (midiToCps' 31.0) :: Pitch
aFlat1 = Pitch (midiToCps' 32.0) :: Pitch
gDoubleSharp1 = Pitch (midiToCps' 33.0) :: Pitch
bDoubleFlat1 = Pitch (midiToCps' 33.0) :: Pitch
bFlat1 = Pitch (midiToCps' 34.0) :: Pitch
cDoubleFlat2 = Pitch (midiToCps' 34.0) :: Pitch
aDoubleSharp1 = Pitch (midiToCps' 35.0) :: Pitch
cFlat2 = Pitch (midiToCps' 35.0) :: Pitch
bSharp1 = Pitch (midiToCps' 36.0) :: Pitch
dDoubleFlat2 = Pitch (midiToCps' 36.0) :: Pitch
bDoubleSharp1 = Pitch (midiToCps' 37.0) :: Pitch
dFlat2 = Pitch (midiToCps' 37.0) :: Pitch
cDoubleSharp2 = Pitch (midiToCps' 38.0) :: Pitch
eDoubleFlat2 = Pitch (midiToCps' 38.0) :: Pitch
eFlat2 = Pitch (midiToCps' 39.0) :: Pitch
fDoubleFlat2 = Pitch (midiToCps' 39.0) :: Pitch
dDoubleSharp2 = Pitch (midiToCps' 40.0) :: Pitch
fFlat2 = Pitch (midiToCps' 40.0) :: Pitch
eSharp2 = Pitch (midiToCps' 41.0) :: Pitch
gDoubleFlat2 = Pitch (midiToCps' 41.0) :: Pitch
eDoubleSharp2 = Pitch (midiToCps' 42.0) :: Pitch
gFlat2 = Pitch (midiToCps' 42.0) :: Pitch
fDoubleSharp2 = Pitch (midiToCps' 43.0) :: Pitch
aDoubleFlat2 = Pitch (midiToCps' 43.0) :: Pitch
aFlat2 = Pitch (midiToCps' 44.0) :: Pitch
gDoubleSharp2 = Pitch (midiToCps' 45.0) :: Pitch
bDoubleFlat2 = Pitch (midiToCps' 45.0) :: Pitch
bFlat2 = Pitch (midiToCps' 46.0) :: Pitch
cDoubleFlat3 = Pitch (midiToCps' 46.0) :: Pitch
aDoubleSharp2 = Pitch (midiToCps' 47.0) :: Pitch
cFlat3 = Pitch (midiToCps' 47.0) :: Pitch
bSharp2 = Pitch (midiToCps' 48.0) :: Pitch
dDoubleFlat3 = Pitch (midiToCps' 48.0) :: Pitch
bDoubleSharp2 = Pitch (midiToCps' 49.0) :: Pitch
dFlat3 = Pitch (midiToCps' 49.0) :: Pitch
cDoubleSharp3 = Pitch (midiToCps' 50.0) :: Pitch
eDoubleFlat3 = Pitch (midiToCps' 50.0) :: Pitch
eFlat3 = Pitch (midiToCps' 51.0) :: Pitch
fDoubleFlat3 = Pitch (midiToCps' 51.0) :: Pitch
dDoubleSharp3 = Pitch (midiToCps' 52.0) :: Pitch
fFlat3 = Pitch (midiToCps' 52.0) :: Pitch
eSharp3 = Pitch (midiToCps' 53.0) :: Pitch
gDoubleFlat3 = Pitch (midiToCps' 53.0) :: Pitch
eDoubleSharp3 = Pitch (midiToCps' 54.0) :: Pitch
gFlat3 = Pitch (midiToCps' 54.0) :: Pitch
fDoubleSharp3 = Pitch (midiToCps' 55.0) :: Pitch
aDoubleFlat3 = Pitch (midiToCps' 55.0) :: Pitch
aFlat3 = Pitch (midiToCps' 56.0) :: Pitch
gDoubleSharp3 = Pitch (midiToCps' 57.0) :: Pitch
bDoubleFlat3 = Pitch (midiToCps' 57.0) :: Pitch
bFlat3 = Pitch (midiToCps' 58.0) :: Pitch
cDoubleFlat4 = Pitch (midiToCps' 58.0) :: Pitch
aDoubleSharp3 = Pitch (midiToCps' 59.0) :: Pitch
cFlat4 = Pitch (midiToCps' 59.0) :: Pitch
bSharp3 = Pitch (midiToCps' 60.0) :: Pitch
dDoubleFlat4 = Pitch (midiToCps' 60.0) :: Pitch
bDoubleSharp3 = Pitch (midiToCps' 61.0) :: Pitch
dFlat4 = Pitch (midiToCps' 61.0) :: Pitch
cDoubleSharp4 = Pitch (midiToCps' 62.0) :: Pitch
eDoubleFlat4 = Pitch (midiToCps' 62.0) :: Pitch
eFlat4 = Pitch (midiToCps' 63.0) :: Pitch
fDoubleFlat4 = Pitch (midiToCps' 63.0) :: Pitch
dDoubleSharp4 = Pitch (midiToCps' 64.0) :: Pitch
fFlat4 = Pitch (midiToCps' 64.0) :: Pitch
eSharp4 = Pitch (midiToCps' 65.0) :: Pitch
gDoubleFlat4 = Pitch (midiToCps' 65.0) :: Pitch
eDoubleSharp4 = Pitch (midiToCps' 66.0) :: Pitch
gFlat4 = Pitch (midiToCps' 66.0) :: Pitch
fDoubleSharp4 = Pitch (midiToCps' 67.0) :: Pitch
aDoubleFlat4 = Pitch (midiToCps' 67.0) :: Pitch
aFlat4 = Pitch (midiToCps' 68.0) :: Pitch
gDoubleSharp4 = Pitch (midiToCps' 69.0) :: Pitch
bDoubleFlat4 = Pitch (midiToCps' 69.0) :: Pitch
bFlat4 = Pitch (midiToCps' 70.0) :: Pitch
cDoubleFlat5 = Pitch (midiToCps' 70.0) :: Pitch
aDoubleSharp4 = Pitch (midiToCps' 71.0) :: Pitch
cFlat5 = Pitch (midiToCps' 71.0) :: Pitch
bSharp4 = Pitch (midiToCps' 72.0) :: Pitch
dDoubleFlat5 = Pitch (midiToCps' 72.0) :: Pitch
bDoubleSharp4 = Pitch (midiToCps' 73.0) :: Pitch
dFlat5 = Pitch (midiToCps' 73.0) :: Pitch
cDoubleSharp5 = Pitch (midiToCps' 74.0) :: Pitch
eDoubleFlat5 = Pitch (midiToCps' 74.0) :: Pitch
eFlat5 = Pitch (midiToCps' 75.0) :: Pitch
fDoubleFlat5 = Pitch (midiToCps' 75.0) :: Pitch
dDoubleSharp5 = Pitch (midiToCps' 76.0) :: Pitch
fFlat5 = Pitch (midiToCps' 76.0) :: Pitch
eSharp5 = Pitch (midiToCps' 77.0) :: Pitch
gDoubleFlat5 = Pitch (midiToCps' 77.0) :: Pitch
eDoubleSharp5 = Pitch (midiToCps' 78.0) :: Pitch
gFlat5 = Pitch (midiToCps' 78.0) :: Pitch
fDoubleSharp5 = Pitch (midiToCps' 79.0) :: Pitch
aDoubleFlat5 = Pitch (midiToCps' 79.0) :: Pitch
aFlat5 = Pitch (midiToCps' 80.0) :: Pitch
gDoubleSharp5 = Pitch (midiToCps' 81.0) :: Pitch
bDoubleFlat5 = Pitch (midiToCps' 81.0) :: Pitch
bFlat5 = Pitch (midiToCps' 82.0) :: Pitch
cDoubleFlat6 = Pitch (midiToCps' 82.0) :: Pitch
aDoubleSharp5 = Pitch (midiToCps' 83.0) :: Pitch
cFlat6 = Pitch (midiToCps' 83.0) :: Pitch
bSharp5 = Pitch (midiToCps' 84.0) :: Pitch
dDoubleFlat6 = Pitch (midiToCps' 84.0) :: Pitch
bDoubleSharp5 = Pitch (midiToCps' 85.0) :: Pitch
dFlat6 = Pitch (midiToCps' 85.0) :: Pitch
cDoubleSharp6 = Pitch (midiToCps' 86.0) :: Pitch
eDoubleFlat6 = Pitch (midiToCps' 86.0) :: Pitch
eFlat6 = Pitch (midiToCps' 87.0) :: Pitch
fDoubleFlat6 = Pitch (midiToCps' 87.0) :: Pitch
dDoubleSharp6 = Pitch (midiToCps' 88.0) :: Pitch
fFlat6 = Pitch (midiToCps' 88.0) :: Pitch
eSharp6 = Pitch (midiToCps' 89.0) :: Pitch
gDoubleFlat6 = Pitch (midiToCps' 89.0) :: Pitch
eDoubleSharp6 = Pitch (midiToCps' 90.0) :: Pitch
gFlat6 = Pitch (midiToCps' 90.0) :: Pitch
fDoubleSharp6 = Pitch (midiToCps' 91.0) :: Pitch
aDoubleFlat6 = Pitch (midiToCps' 91.0) :: Pitch
aFlat6 = Pitch (midiToCps' 92.0) :: Pitch
gDoubleSharp6 = Pitch (midiToCps' 93.0) :: Pitch
bDoubleFlat6 = Pitch (midiToCps' 93.0) :: Pitch
bFlat6 = Pitch (midiToCps' 94.0) :: Pitch
cDoubleFlat7 = Pitch (midiToCps' 94.0) :: Pitch
aDoubleSharp6 = Pitch (midiToCps' 95.0) :: Pitch
cFlat7 = Pitch (midiToCps' 95.0) :: Pitch
bSharp6 = Pitch (midiToCps' 96.0) :: Pitch
dDoubleFlat7 = Pitch (midiToCps' 96.0) :: Pitch
bDoubleSharp6 = Pitch (midiToCps' 97.0) :: Pitch
dFlat7 = Pitch (midiToCps' 97.0) :: Pitch
cDoubleSharp7 = Pitch (midiToCps' 98.0) :: Pitch
eDoubleFlat7 = Pitch (midiToCps' 98.0) :: Pitch
eFlat7 = Pitch (midiToCps' 99.0) :: Pitch
fDoubleFlat7 = Pitch (midiToCps' 99.0) :: Pitch
dDoubleSharp7 = Pitch (midiToCps' 100.0) :: Pitch
fFlat7 = Pitch (midiToCps' 100.0) :: Pitch
eSharp7 = Pitch (midiToCps' 101.0) :: Pitch
gDoubleFlat7 = Pitch (midiToCps' 101.0) :: Pitch
eDoubleSharp7 = Pitch (midiToCps' 102.0) :: Pitch
gFlat7 = Pitch (midiToCps' 102.0) :: Pitch
fDoubleSharp7 = Pitch (midiToCps' 103.0) :: Pitch
aDoubleFlat7 = Pitch (midiToCps' 103.0) :: Pitch
aFlat7 = Pitch (midiToCps' 104.0) :: Pitch
gDoubleSharp7 = Pitch (midiToCps' 105.0) :: Pitch
bDoubleFlat7 = Pitch (midiToCps' 105.0) :: Pitch
bFlat7 = Pitch (midiToCps' 106.0) :: Pitch
cDoubleFlat8 = Pitch (midiToCps' 106.0) :: Pitch
aDoubleSharp7 = Pitch (midiToCps' 107.0) :: Pitch
cFlat8 = Pitch (midiToCps' 107.0) :: Pitch

middleC = c4 :: Pitch


transpose :: Pitch -> Pitch -> Pitch
transpose = add

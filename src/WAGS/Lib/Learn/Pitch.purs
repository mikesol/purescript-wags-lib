module WAGS.Lib.Learn.Pitch where

import Prelude

import Data.Identity (Identity(..))
import Data.Int (round, toNumber)
import Data.Newtype (class Newtype, unwrap)
import Math (log, pow)
import Safe.Coerce (coerce)

type Time = Number
newtype Pitch f = Pitch (f Number)

derive instance newtypePitch :: Newtype (Pitch f) _
derive newtype instance eqPitch :: Eq (f Number) => Eq (Pitch f)
derive newtype instance ordPitch :: Ord (f Number) => Ord (Pitch f)
derive newtype instance boundedPitch :: Bounded (f Number) => Bounded (Pitch f)
derive newtype instance showPitch :: Show (f Number) => Show (Pitch f)
instance commutativeRingPitch :: Applicative f => CommutativeRing (Pitch f)
instance euclideanRingPitch :: Applicative f => EuclideanRing (Pitch f) where
  degree _ = 1
  div (Pitch a) (Pitch b) = Pitch (midiToCps' <$> (div <$> (cpsToMidi' <$> a) <*> (cpsToMidi' <$> b)))
  mod (Pitch a) (Pitch b) = Pitch (midiToCps' <$> (mod <$> (cpsToMidi' <$> a) <*> (cpsToMidi' <$> b)))
instance ringPitch :: Applicative f => Ring (Pitch f) where
  sub (Pitch a) (Pitch b) = Pitch (midiToCps' <$> (sub <$> (cpsToMidi' <$> a) <*> (cpsToMidi' <$> b)))
instance semiringPitch :: Applicative f => Semiring (Pitch f) where
  zero = Pitch (pure zero)
  one = Pitch (pure one)
  add (Pitch a) (Pitch b) = Pitch (midiToCps' <$> (add <$> (cpsToMidi' <$> a) <*> (cpsToMidi' <$> b)))
  mul (Pitch a) (Pitch b) = Pitch (midiToCps' <$> (mul <$> (cpsToMidi' <$> a) <*> (cpsToMidi' <$> b)))

midiToPitch :: Int -> Pitch Identity
midiToPitch = coerce <<< midiToCps

midiToCps :: Int -> Number
midiToCps = midiToCps' <<< toNumber

midiToCps' :: Number -> Number
midiToCps' i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

cpsToMidi' :: Number -> Number
cpsToMidi' i = (log (i / 440.0) / log 2.0) * 12.0 + 69.0

cpsToMidi :: Number -> Int
cpsToMidi = round <<< cpsToMidi'

pitchToMidi :: Pitch Identity -> Int
pitchToMidi = cpsToMidi <<< coerce

fot :: (Time -> Pitch Identity) -> Pitch (Function Time)
fot = Pitch <<< compose (unwrap <<< unwrap)

fot2 :: (Time -> Number -> Number) -> Pitch Identity -> Pitch (Function Time)
fot2 f (Pitch (Identity p)) = Pitch \t -> f t p

semitone = Pitch $ Identity (midiToCps' 1.0) :: Pitch Identity
wholeTone = Pitch $ Identity (midiToCps' 2.0) :: Pitch Identity
minorSecond = Pitch $ Identity (midiToCps' 1.0) :: Pitch Identity
majorSecond = Pitch $ Identity (midiToCps' 2.0) :: Pitch Identity
minorThird = Pitch $ Identity (midiToCps' 3.0) :: Pitch Identity
majorThird = Pitch $ Identity (midiToCps' 4.0) :: Pitch Identity
fourth = Pitch $ Identity (midiToCps' 5.0) :: Pitch Identity
perfectFourth = Pitch $ Identity (midiToCps' 5.0) :: Pitch Identity
augmentedFourth = Pitch $ Identity (midiToCps' 6.0) :: Pitch Identity
diminishedFifth = Pitch $ Identity (midiToCps' 6.0) :: Pitch Identity
fifth = Pitch $ Identity (midiToCps' 7.0) :: Pitch Identity
perfectFifth = Pitch $ Identity (midiToCps' 7.0) :: Pitch Identity
minorSixth = Pitch $ Identity (midiToCps' 8.0) :: Pitch Identity
majorSixth = Pitch $ Identity (midiToCps' 9.0) :: Pitch Identity
minorSeventh = Pitch $ Identity (midiToCps' 10.0) :: Pitch Identity
majorSeventh = Pitch $ Identity (midiToCps' 11.0) :: Pitch Identity
octave = Pitch $ Identity (midiToCps' 12.0) :: Pitch Identity
perfectOctave = Pitch $ Identity (midiToCps' 12.0) :: Pitch Identity

c0 = Pitch $ Identity (midiToCps' 12.0) :: Pitch Identity
cSharp0 = Pitch $ Identity (midiToCps' 13.0) :: Pitch Identity
d0 = Pitch $ Identity (midiToCps' 14.0) :: Pitch Identity
dSharp0 = Pitch $ Identity (midiToCps' 15.0) :: Pitch Identity
e0 = Pitch $ Identity (midiToCps' 16.0) :: Pitch Identity
f0 = Pitch $ Identity (midiToCps' 17.0) :: Pitch Identity
fSharp0 = Pitch $ Identity (midiToCps' 18.0) :: Pitch Identity
g0 = Pitch $ Identity (midiToCps' 19.0) :: Pitch Identity
gSharp0 = Pitch $ Identity (midiToCps' 20.0) :: Pitch Identity
a0 = Pitch $ Identity (midiToCps' 21.0) :: Pitch Identity
aSharp0 = Pitch $ Identity (midiToCps' 22.0) :: Pitch Identity
b0 = Pitch $ Identity (midiToCps' 23.0) :: Pitch Identity
c1 = Pitch $ Identity (midiToCps' 24.0) :: Pitch Identity
cSharp1 = Pitch $ Identity (midiToCps' 25.0) :: Pitch Identity
d1 = Pitch $ Identity (midiToCps' 26.0) :: Pitch Identity
dSharp1 = Pitch $ Identity (midiToCps' 27.0) :: Pitch Identity
e1 = Pitch $ Identity (midiToCps' 28.0) :: Pitch Identity
f1 = Pitch $ Identity (midiToCps' 29.0) :: Pitch Identity
fSharp1 = Pitch $ Identity (midiToCps' 30.0) :: Pitch Identity
g1 = Pitch $ Identity (midiToCps' 31.0) :: Pitch Identity
gSharp1 = Pitch $ Identity (midiToCps' 32.0) :: Pitch Identity
a1 = Pitch $ Identity (midiToCps' 33.0) :: Pitch Identity
aSharp1 = Pitch $ Identity (midiToCps' 34.0) :: Pitch Identity
b1 = Pitch $ Identity (midiToCps' 35.0) :: Pitch Identity
c2 = Pitch $ Identity (midiToCps' 36.0) :: Pitch Identity
cSharp2 = Pitch $ Identity (midiToCps' 37.0) :: Pitch Identity
d2 = Pitch $ Identity (midiToCps' 38.0) :: Pitch Identity
dSharp2 = Pitch $ Identity (midiToCps' 39.0) :: Pitch Identity
e2 = Pitch $ Identity (midiToCps' 40.0) :: Pitch Identity
f2 = Pitch $ Identity (midiToCps' 41.0) :: Pitch Identity
fSharp2 = Pitch $ Identity (midiToCps' 42.0) :: Pitch Identity
g2 = Pitch $ Identity (midiToCps' 43.0) :: Pitch Identity
gSharp2 = Pitch $ Identity (midiToCps' 44.0) :: Pitch Identity
a2 = Pitch $ Identity (midiToCps' 45.0) :: Pitch Identity
aSharp2 = Pitch $ Identity (midiToCps' 46.0) :: Pitch Identity
b2 = Pitch $ Identity (midiToCps' 47.0) :: Pitch Identity
c3 = Pitch $ Identity (midiToCps' 48.0) :: Pitch Identity
cSharp3 = Pitch $ Identity (midiToCps' 49.0) :: Pitch Identity
d3 = Pitch $ Identity (midiToCps' 50.0) :: Pitch Identity
dSharp3 = Pitch $ Identity (midiToCps' 51.0) :: Pitch Identity
e3 = Pitch $ Identity (midiToCps' 52.0) :: Pitch Identity
f3 = Pitch $ Identity (midiToCps' 53.0) :: Pitch Identity
fSharp3 = Pitch $ Identity (midiToCps' 54.0) :: Pitch Identity
g3 = Pitch $ Identity (midiToCps' 55.0) :: Pitch Identity
gSharp3 = Pitch $ Identity (midiToCps' 56.0) :: Pitch Identity
a3 = Pitch $ Identity (midiToCps' 57.0) :: Pitch Identity
aSharp3 = Pitch $ Identity (midiToCps' 58.0) :: Pitch Identity
b3 = Pitch $ Identity (midiToCps' 59.0) :: Pitch Identity
c4 = Pitch $ Identity (midiToCps' 60.0) :: Pitch Identity
cSharp4 = Pitch $ Identity (midiToCps' 61.0) :: Pitch Identity
d4 = Pitch $ Identity (midiToCps' 62.0) :: Pitch Identity
dSharp4 = Pitch $ Identity (midiToCps' 63.0) :: Pitch Identity
e4 = Pitch $ Identity (midiToCps' 64.0) :: Pitch Identity
f4 = Pitch $ Identity (midiToCps' 65.0) :: Pitch Identity
fSharp4 = Pitch $ Identity (midiToCps' 66.0) :: Pitch Identity
g4 = Pitch $ Identity (midiToCps' 67.0) :: Pitch Identity
gSharp4 = Pitch $ Identity (midiToCps' 68.0) :: Pitch Identity
a4 = Pitch $ Identity (midiToCps' 69.0) :: Pitch Identity
aSharp4 = Pitch $ Identity (midiToCps' 70.0) :: Pitch Identity
b4 = Pitch $ Identity (midiToCps' 71.0) :: Pitch Identity
c5 = Pitch $ Identity (midiToCps' 72.0) :: Pitch Identity
cSharp5 = Pitch $ Identity (midiToCps' 73.0) :: Pitch Identity
d5 = Pitch $ Identity (midiToCps' 74.0) :: Pitch Identity
dSharp5 = Pitch $ Identity (midiToCps' 75.0) :: Pitch Identity
e5 = Pitch $ Identity (midiToCps' 76.0) :: Pitch Identity
f5 = Pitch $ Identity (midiToCps' 77.0) :: Pitch Identity
fSharp5 = Pitch $ Identity (midiToCps' 78.0) :: Pitch Identity
g5 = Pitch $ Identity (midiToCps' 79.0) :: Pitch Identity
gSharp5 = Pitch $ Identity (midiToCps' 80.0) :: Pitch Identity
a5 = Pitch $ Identity (midiToCps' 81.0) :: Pitch Identity
aSharp5 = Pitch $ Identity (midiToCps' 82.0) :: Pitch Identity
b5 = Pitch $ Identity (midiToCps' 83.0) :: Pitch Identity
c6 = Pitch $ Identity (midiToCps' 84.0) :: Pitch Identity
cSharp6 = Pitch $ Identity (midiToCps' 85.0) :: Pitch Identity
d6 = Pitch $ Identity (midiToCps' 86.0) :: Pitch Identity
dSharp6 = Pitch $ Identity (midiToCps' 87.0) :: Pitch Identity
e6 = Pitch $ Identity (midiToCps' 88.0) :: Pitch Identity
f6 = Pitch $ Identity (midiToCps' 89.0) :: Pitch Identity
fSharp6 = Pitch $ Identity (midiToCps' 90.0) :: Pitch Identity
g6 = Pitch $ Identity (midiToCps' 91.0) :: Pitch Identity
gSharp6 = Pitch $ Identity (midiToCps' 92.0) :: Pitch Identity
a6 = Pitch $ Identity (midiToCps' 93.0) :: Pitch Identity
aSharp6 = Pitch $ Identity (midiToCps' 94.0) :: Pitch Identity
b6 = Pitch $ Identity (midiToCps' 95.0) :: Pitch Identity
c7 = Pitch $ Identity (midiToCps' 96.0) :: Pitch Identity
cSharp7 = Pitch $ Identity (midiToCps' 97.0) :: Pitch Identity
d7 = Pitch $ Identity (midiToCps' 98.0) :: Pitch Identity
dSharp7 = Pitch $ Identity (midiToCps' 99.0) :: Pitch Identity
e7 = Pitch $ Identity (midiToCps' 100.0) :: Pitch Identity
f7 = Pitch $ Identity (midiToCps' 101.0) :: Pitch Identity
fSharp7 = Pitch $ Identity (midiToCps' 102.0) :: Pitch Identity
g7 = Pitch $ Identity (midiToCps' 103.0) :: Pitch Identity
gSharp7 = Pitch $ Identity (midiToCps' 104.0) :: Pitch Identity
a7 = Pitch $ Identity (midiToCps' 105.0) :: Pitch Identity
aSharp7 = Pitch $ Identity (midiToCps' 106.0) :: Pitch Identity
b7 = Pitch $ Identity (midiToCps' 107.0) :: Pitch Identity
dDoubleFlat0 = Pitch $ Identity (midiToCps' 12.0) :: Pitch Identity
dFlat0 = Pitch $ Identity (midiToCps' 13.0) :: Pitch Identity
cDoubleSharp0 = Pitch $ Identity (midiToCps' 14.0) :: Pitch Identity
eDoubleFlat0 = Pitch $ Identity (midiToCps' 14.0) :: Pitch Identity
eFlat0 = Pitch $ Identity (midiToCps' 15.0) :: Pitch Identity
fDoubleFlat0 = Pitch $ Identity (midiToCps' 15.0) :: Pitch Identity
dDoubleSharp0 = Pitch $ Identity (midiToCps' 16.0) :: Pitch Identity
fFlat0 = Pitch $ Identity (midiToCps' 16.0) :: Pitch Identity
eSharp0 = Pitch $ Identity (midiToCps' 17.0) :: Pitch Identity
gDoubleFlat0 = Pitch $ Identity (midiToCps' 17.0) :: Pitch Identity
eDoubleSharp0 = Pitch $ Identity (midiToCps' 18.0) :: Pitch Identity
gFlat0 = Pitch $ Identity (midiToCps' 18.0) :: Pitch Identity
fDoubleSharp0 = Pitch $ Identity (midiToCps' 19.0) :: Pitch Identity
aDoubleFlat0 = Pitch $ Identity (midiToCps' 19.0) :: Pitch Identity
aFlat0 = Pitch $ Identity (midiToCps' 20.0) :: Pitch Identity
gDoubleSharp0 = Pitch $ Identity (midiToCps' 21.0) :: Pitch Identity
bDoubleFlat0 = Pitch $ Identity (midiToCps' 21.0) :: Pitch Identity
bFlat0 = Pitch $ Identity (midiToCps' 22.0) :: Pitch Identity
cDoubleFlat1 = Pitch $ Identity (midiToCps' 22.0) :: Pitch Identity
aDoubleSharp0 = Pitch $ Identity (midiToCps' 23.0) :: Pitch Identity
cFlat1 = Pitch $ Identity (midiToCps' 23.0) :: Pitch Identity
bSharp0 = Pitch $ Identity (midiToCps' 24.0) :: Pitch Identity
dDoubleFlat1 = Pitch $ Identity (midiToCps' 24.0) :: Pitch Identity
bDoubleSharp0 = Pitch $ Identity (midiToCps' 25.0) :: Pitch Identity
dFlat1 = Pitch $ Identity (midiToCps' 25.0) :: Pitch Identity
cDoubleSharp1 = Pitch $ Identity (midiToCps' 26.0) :: Pitch Identity
eDoubleFlat1 = Pitch $ Identity (midiToCps' 26.0) :: Pitch Identity
eFlat1 = Pitch $ Identity (midiToCps' 27.0) :: Pitch Identity
fDoubleFlat1 = Pitch $ Identity (midiToCps' 27.0) :: Pitch Identity
dDoubleSharp1 = Pitch $ Identity (midiToCps' 28.0) :: Pitch Identity
fFlat1 = Pitch $ Identity (midiToCps' 28.0) :: Pitch Identity
eSharp1 = Pitch $ Identity (midiToCps' 29.0) :: Pitch Identity
gDoubleFlat1 = Pitch $ Identity (midiToCps' 29.0) :: Pitch Identity
eDoubleSharp1 = Pitch $ Identity (midiToCps' 30.0) :: Pitch Identity
gFlat1 = Pitch $ Identity (midiToCps' 30.0) :: Pitch Identity
fDoubleSharp1 = Pitch $ Identity (midiToCps' 31.0) :: Pitch Identity
aDoubleFlat1 = Pitch $ Identity (midiToCps' 31.0) :: Pitch Identity
aFlat1 = Pitch $ Identity (midiToCps' 32.0) :: Pitch Identity
gDoubleSharp1 = Pitch $ Identity (midiToCps' 33.0) :: Pitch Identity
bDoubleFlat1 = Pitch $ Identity (midiToCps' 33.0) :: Pitch Identity
bFlat1 = Pitch $ Identity (midiToCps' 34.0) :: Pitch Identity
cDoubleFlat2 = Pitch $ Identity (midiToCps' 34.0) :: Pitch Identity
aDoubleSharp1 = Pitch $ Identity (midiToCps' 35.0) :: Pitch Identity
cFlat2 = Pitch $ Identity (midiToCps' 35.0) :: Pitch Identity
bSharp1 = Pitch $ Identity (midiToCps' 36.0) :: Pitch Identity
dDoubleFlat2 = Pitch $ Identity (midiToCps' 36.0) :: Pitch Identity
bDoubleSharp1 = Pitch $ Identity (midiToCps' 37.0) :: Pitch Identity
dFlat2 = Pitch $ Identity (midiToCps' 37.0) :: Pitch Identity
cDoubleSharp2 = Pitch $ Identity (midiToCps' 38.0) :: Pitch Identity
eDoubleFlat2 = Pitch $ Identity (midiToCps' 38.0) :: Pitch Identity
eFlat2 = Pitch $ Identity (midiToCps' 39.0) :: Pitch Identity
fDoubleFlat2 = Pitch $ Identity (midiToCps' 39.0) :: Pitch Identity
dDoubleSharp2 = Pitch $ Identity (midiToCps' 40.0) :: Pitch Identity
fFlat2 = Pitch $ Identity (midiToCps' 40.0) :: Pitch Identity
eSharp2 = Pitch $ Identity (midiToCps' 41.0) :: Pitch Identity
gDoubleFlat2 = Pitch $ Identity (midiToCps' 41.0) :: Pitch Identity
eDoubleSharp2 = Pitch $ Identity (midiToCps' 42.0) :: Pitch Identity
gFlat2 = Pitch $ Identity (midiToCps' 42.0) :: Pitch Identity
fDoubleSharp2 = Pitch $ Identity (midiToCps' 43.0) :: Pitch Identity
aDoubleFlat2 = Pitch $ Identity (midiToCps' 43.0) :: Pitch Identity
aFlat2 = Pitch $ Identity (midiToCps' 44.0) :: Pitch Identity
gDoubleSharp2 = Pitch $ Identity (midiToCps' 45.0) :: Pitch Identity
bDoubleFlat2 = Pitch $ Identity (midiToCps' 45.0) :: Pitch Identity
bFlat2 = Pitch $ Identity (midiToCps' 46.0) :: Pitch Identity
cDoubleFlat3 = Pitch $ Identity (midiToCps' 46.0) :: Pitch Identity
aDoubleSharp2 = Pitch $ Identity (midiToCps' 47.0) :: Pitch Identity
cFlat3 = Pitch $ Identity (midiToCps' 47.0) :: Pitch Identity
bSharp2 = Pitch $ Identity (midiToCps' 48.0) :: Pitch Identity
dDoubleFlat3 = Pitch $ Identity (midiToCps' 48.0) :: Pitch Identity
bDoubleSharp2 = Pitch $ Identity (midiToCps' 49.0) :: Pitch Identity
dFlat3 = Pitch $ Identity (midiToCps' 49.0) :: Pitch Identity
cDoubleSharp3 = Pitch $ Identity (midiToCps' 50.0) :: Pitch Identity
eDoubleFlat3 = Pitch $ Identity (midiToCps' 50.0) :: Pitch Identity
eFlat3 = Pitch $ Identity (midiToCps' 51.0) :: Pitch Identity
fDoubleFlat3 = Pitch $ Identity (midiToCps' 51.0) :: Pitch Identity
dDoubleSharp3 = Pitch $ Identity (midiToCps' 52.0) :: Pitch Identity
fFlat3 = Pitch $ Identity (midiToCps' 52.0) :: Pitch Identity
eSharp3 = Pitch $ Identity (midiToCps' 53.0) :: Pitch Identity
gDoubleFlat3 = Pitch $ Identity (midiToCps' 53.0) :: Pitch Identity
eDoubleSharp3 = Pitch $ Identity (midiToCps' 54.0) :: Pitch Identity
gFlat3 = Pitch $ Identity (midiToCps' 54.0) :: Pitch Identity
fDoubleSharp3 = Pitch $ Identity (midiToCps' 55.0) :: Pitch Identity
aDoubleFlat3 = Pitch $ Identity (midiToCps' 55.0) :: Pitch Identity
aFlat3 = Pitch $ Identity (midiToCps' 56.0) :: Pitch Identity
gDoubleSharp3 = Pitch $ Identity (midiToCps' 57.0) :: Pitch Identity
bDoubleFlat3 = Pitch $ Identity (midiToCps' 57.0) :: Pitch Identity
bFlat3 = Pitch $ Identity (midiToCps' 58.0) :: Pitch Identity
cDoubleFlat4 = Pitch $ Identity (midiToCps' 58.0) :: Pitch Identity
aDoubleSharp3 = Pitch $ Identity (midiToCps' 59.0) :: Pitch Identity
cFlat4 = Pitch $ Identity (midiToCps' 59.0) :: Pitch Identity
bSharp3 = Pitch $ Identity (midiToCps' 60.0) :: Pitch Identity
dDoubleFlat4 = Pitch $ Identity (midiToCps' 60.0) :: Pitch Identity
bDoubleSharp3 = Pitch $ Identity (midiToCps' 61.0) :: Pitch Identity
dFlat4 = Pitch $ Identity (midiToCps' 61.0) :: Pitch Identity
cDoubleSharp4 = Pitch $ Identity (midiToCps' 62.0) :: Pitch Identity
eDoubleFlat4 = Pitch $ Identity (midiToCps' 62.0) :: Pitch Identity
eFlat4 = Pitch $ Identity (midiToCps' 63.0) :: Pitch Identity
fDoubleFlat4 = Pitch $ Identity (midiToCps' 63.0) :: Pitch Identity
dDoubleSharp4 = Pitch $ Identity (midiToCps' 64.0) :: Pitch Identity
fFlat4 = Pitch $ Identity (midiToCps' 64.0) :: Pitch Identity
eSharp4 = Pitch $ Identity (midiToCps' 65.0) :: Pitch Identity
gDoubleFlat4 = Pitch $ Identity (midiToCps' 65.0) :: Pitch Identity
eDoubleSharp4 = Pitch $ Identity (midiToCps' 66.0) :: Pitch Identity
gFlat4 = Pitch $ Identity (midiToCps' 66.0) :: Pitch Identity
fDoubleSharp4 = Pitch $ Identity (midiToCps' 67.0) :: Pitch Identity
aDoubleFlat4 = Pitch $ Identity (midiToCps' 67.0) :: Pitch Identity
aFlat4 = Pitch $ Identity (midiToCps' 68.0) :: Pitch Identity
gDoubleSharp4 = Pitch $ Identity (midiToCps' 69.0) :: Pitch Identity
bDoubleFlat4 = Pitch $ Identity (midiToCps' 69.0) :: Pitch Identity
bFlat4 = Pitch $ Identity (midiToCps' 70.0) :: Pitch Identity
cDoubleFlat5 = Pitch $ Identity (midiToCps' 70.0) :: Pitch Identity
aDoubleSharp4 = Pitch $ Identity (midiToCps' 71.0) :: Pitch Identity
cFlat5 = Pitch $ Identity (midiToCps' 71.0) :: Pitch Identity
bSharp4 = Pitch $ Identity (midiToCps' 72.0) :: Pitch Identity
dDoubleFlat5 = Pitch $ Identity (midiToCps' 72.0) :: Pitch Identity
bDoubleSharp4 = Pitch $ Identity (midiToCps' 73.0) :: Pitch Identity
dFlat5 = Pitch $ Identity (midiToCps' 73.0) :: Pitch Identity
cDoubleSharp5 = Pitch $ Identity (midiToCps' 74.0) :: Pitch Identity
eDoubleFlat5 = Pitch $ Identity (midiToCps' 74.0) :: Pitch Identity
eFlat5 = Pitch $ Identity (midiToCps' 75.0) :: Pitch Identity
fDoubleFlat5 = Pitch $ Identity (midiToCps' 75.0) :: Pitch Identity
dDoubleSharp5 = Pitch $ Identity (midiToCps' 76.0) :: Pitch Identity
fFlat5 = Pitch $ Identity (midiToCps' 76.0) :: Pitch Identity
eSharp5 = Pitch $ Identity (midiToCps' 77.0) :: Pitch Identity
gDoubleFlat5 = Pitch $ Identity (midiToCps' 77.0) :: Pitch Identity
eDoubleSharp5 = Pitch $ Identity (midiToCps' 78.0) :: Pitch Identity
gFlat5 = Pitch $ Identity (midiToCps' 78.0) :: Pitch Identity
fDoubleSharp5 = Pitch $ Identity (midiToCps' 79.0) :: Pitch Identity
aDoubleFlat5 = Pitch $ Identity (midiToCps' 79.0) :: Pitch Identity
aFlat5 = Pitch $ Identity (midiToCps' 80.0) :: Pitch Identity
gDoubleSharp5 = Pitch $ Identity (midiToCps' 81.0) :: Pitch Identity
bDoubleFlat5 = Pitch $ Identity (midiToCps' 81.0) :: Pitch Identity
bFlat5 = Pitch $ Identity (midiToCps' 82.0) :: Pitch Identity
cDoubleFlat6 = Pitch $ Identity (midiToCps' 82.0) :: Pitch Identity
aDoubleSharp5 = Pitch $ Identity (midiToCps' 83.0) :: Pitch Identity
cFlat6 = Pitch $ Identity (midiToCps' 83.0) :: Pitch Identity
bSharp5 = Pitch $ Identity (midiToCps' 84.0) :: Pitch Identity
dDoubleFlat6 = Pitch $ Identity (midiToCps' 84.0) :: Pitch Identity
bDoubleSharp5 = Pitch $ Identity (midiToCps' 85.0) :: Pitch Identity
dFlat6 = Pitch $ Identity (midiToCps' 85.0) :: Pitch Identity
cDoubleSharp6 = Pitch $ Identity (midiToCps' 86.0) :: Pitch Identity
eDoubleFlat6 = Pitch $ Identity (midiToCps' 86.0) :: Pitch Identity
eFlat6 = Pitch $ Identity (midiToCps' 87.0) :: Pitch Identity
fDoubleFlat6 = Pitch $ Identity (midiToCps' 87.0) :: Pitch Identity
dDoubleSharp6 = Pitch $ Identity (midiToCps' 88.0) :: Pitch Identity
fFlat6 = Pitch $ Identity (midiToCps' 88.0) :: Pitch Identity
eSharp6 = Pitch $ Identity (midiToCps' 89.0) :: Pitch Identity
gDoubleFlat6 = Pitch $ Identity (midiToCps' 89.0) :: Pitch Identity
eDoubleSharp6 = Pitch $ Identity (midiToCps' 90.0) :: Pitch Identity
gFlat6 = Pitch $ Identity (midiToCps' 90.0) :: Pitch Identity
fDoubleSharp6 = Pitch $ Identity (midiToCps' 91.0) :: Pitch Identity
aDoubleFlat6 = Pitch $ Identity (midiToCps' 91.0) :: Pitch Identity
aFlat6 = Pitch $ Identity (midiToCps' 92.0) :: Pitch Identity
gDoubleSharp6 = Pitch $ Identity (midiToCps' 93.0) :: Pitch Identity
bDoubleFlat6 = Pitch $ Identity (midiToCps' 93.0) :: Pitch Identity
bFlat6 = Pitch $ Identity (midiToCps' 94.0) :: Pitch Identity
cDoubleFlat7 = Pitch $ Identity (midiToCps' 94.0) :: Pitch Identity
aDoubleSharp6 = Pitch $ Identity (midiToCps' 95.0) :: Pitch Identity
cFlat7 = Pitch $ Identity (midiToCps' 95.0) :: Pitch Identity
bSharp6 = Pitch $ Identity (midiToCps' 96.0) :: Pitch Identity
dDoubleFlat7 = Pitch $ Identity (midiToCps' 96.0) :: Pitch Identity
bDoubleSharp6 = Pitch $ Identity (midiToCps' 97.0) :: Pitch Identity
dFlat7 = Pitch $ Identity (midiToCps' 97.0) :: Pitch Identity
cDoubleSharp7 = Pitch $ Identity (midiToCps' 98.0) :: Pitch Identity
eDoubleFlat7 = Pitch $ Identity (midiToCps' 98.0) :: Pitch Identity
eFlat7 = Pitch $ Identity (midiToCps' 99.0) :: Pitch Identity
fDoubleFlat7 = Pitch $ Identity (midiToCps' 99.0) :: Pitch Identity
dDoubleSharp7 = Pitch $ Identity (midiToCps' 100.0) :: Pitch Identity
fFlat7 = Pitch $ Identity (midiToCps' 100.0) :: Pitch Identity
eSharp7 = Pitch $ Identity (midiToCps' 101.0) :: Pitch Identity
gDoubleFlat7 = Pitch $ Identity (midiToCps' 101.0) :: Pitch Identity
eDoubleSharp7 = Pitch $ Identity (midiToCps' 102.0) :: Pitch Identity
gFlat7 = Pitch $ Identity (midiToCps' 102.0) :: Pitch Identity
fDoubleSharp7 = Pitch $ Identity (midiToCps' 103.0) :: Pitch Identity
aDoubleFlat7 = Pitch $ Identity (midiToCps' 103.0) :: Pitch Identity
aFlat7 = Pitch $ Identity (midiToCps' 104.0) :: Pitch Identity
gDoubleSharp7 = Pitch $ Identity (midiToCps' 105.0) :: Pitch Identity
bDoubleFlat7 = Pitch $ Identity (midiToCps' 105.0) :: Pitch Identity
bFlat7 = Pitch $ Identity (midiToCps' 106.0) :: Pitch Identity
cDoubleFlat8 = Pitch $ Identity (midiToCps' 106.0) :: Pitch Identity
aDoubleSharp7 = Pitch $ Identity (midiToCps' 107.0) :: Pitch Identity
cFlat8 = Pitch $ Identity (midiToCps' 107.0) :: Pitch Identity

middleC = c4 :: Pitch Identity

-------
c0' = Pitch $ pure (midiToCps' 12.0) :: forall f. Applicative f => Pitch f
cSharp0' = Pitch $ pure (midiToCps' 13.0) :: forall f. Applicative f => Pitch f
d0' = Pitch $ pure (midiToCps' 14.0) :: forall f. Applicative f => Pitch f
dSharp0' = Pitch $ pure (midiToCps' 15.0) :: forall f. Applicative f => Pitch f
e0' = Pitch $ pure (midiToCps' 16.0) :: forall f. Applicative f => Pitch f
f0' = Pitch $ pure (midiToCps' 17.0) :: forall f. Applicative f => Pitch f
fSharp0' = Pitch $ pure (midiToCps' 18.0) :: forall f. Applicative f => Pitch f
g0' = Pitch $ pure (midiToCps' 19.0) :: forall f. Applicative f => Pitch f
gSharp0' = Pitch $ pure (midiToCps' 20.0) :: forall f. Applicative f => Pitch f
a0' = Pitch $ pure (midiToCps' 21.0) :: forall f. Applicative f => Pitch f
aSharp0' = Pitch $ pure (midiToCps' 22.0) :: forall f. Applicative f => Pitch f
b0' = Pitch $ pure (midiToCps' 23.0) :: forall f. Applicative f => Pitch f
c1' = Pitch $ pure (midiToCps' 24.0) :: forall f. Applicative f => Pitch f
cSharp1' = Pitch $ pure (midiToCps' 25.0) :: forall f. Applicative f => Pitch f
d1' = Pitch $ pure (midiToCps' 26.0) :: forall f. Applicative f => Pitch f
dSharp1' = Pitch $ pure (midiToCps' 27.0) :: forall f. Applicative f => Pitch f
e1' = Pitch $ pure (midiToCps' 28.0) :: forall f. Applicative f => Pitch f
f1' = Pitch $ pure (midiToCps' 29.0) :: forall f. Applicative f => Pitch f
fSharp1' = Pitch $ pure (midiToCps' 30.0) :: forall f. Applicative f => Pitch f
g1' = Pitch $ pure (midiToCps' 31.0) :: forall f. Applicative f => Pitch f
gSharp1' = Pitch $ pure (midiToCps' 32.0) :: forall f. Applicative f => Pitch f
a1' = Pitch $ pure (midiToCps' 33.0) :: forall f. Applicative f => Pitch f
aSharp1' = Pitch $ pure (midiToCps' 34.0) :: forall f. Applicative f => Pitch f
b1' = Pitch $ pure (midiToCps' 35.0) :: forall f. Applicative f => Pitch f
c2' = Pitch $ pure (midiToCps' 36.0) :: forall f. Applicative f => Pitch f
cSharp2' = Pitch $ pure (midiToCps' 37.0) :: forall f. Applicative f => Pitch f
d2' = Pitch $ pure (midiToCps' 38.0) :: forall f. Applicative f => Pitch f
dSharp2' = Pitch $ pure (midiToCps' 39.0) :: forall f. Applicative f => Pitch f
e2' = Pitch $ pure (midiToCps' 40.0) :: forall f. Applicative f => Pitch f
f2' = Pitch $ pure (midiToCps' 41.0) :: forall f. Applicative f => Pitch f
fSharp2' = Pitch $ pure (midiToCps' 42.0) :: forall f. Applicative f => Pitch f
g2' = Pitch $ pure (midiToCps' 43.0) :: forall f. Applicative f => Pitch f
gSharp2' = Pitch $ pure (midiToCps' 44.0) :: forall f. Applicative f => Pitch f
a2' = Pitch $ pure (midiToCps' 45.0) :: forall f. Applicative f => Pitch f
aSharp2' = Pitch $ pure (midiToCps' 46.0) :: forall f. Applicative f => Pitch f
b2' = Pitch $ pure (midiToCps' 47.0) :: forall f. Applicative f => Pitch f
c3' = Pitch $ pure (midiToCps' 48.0) :: forall f. Applicative f => Pitch f
cSharp3' = Pitch $ pure (midiToCps' 49.0) :: forall f. Applicative f => Pitch f
d3' = Pitch $ pure (midiToCps' 50.0) :: forall f. Applicative f => Pitch f
dSharp3' = Pitch $ pure (midiToCps' 51.0) :: forall f. Applicative f => Pitch f
e3' = Pitch $ pure (midiToCps' 52.0) :: forall f. Applicative f => Pitch f
f3' = Pitch $ pure (midiToCps' 53.0) :: forall f. Applicative f => Pitch f
fSharp3' = Pitch $ pure (midiToCps' 54.0) :: forall f. Applicative f => Pitch f
g3' = Pitch $ pure (midiToCps' 55.0) :: forall f. Applicative f => Pitch f
gSharp3' = Pitch $ pure (midiToCps' 56.0) :: forall f. Applicative f => Pitch f
a3' = Pitch $ pure (midiToCps' 57.0) :: forall f. Applicative f => Pitch f
aSharp3' = Pitch $ pure (midiToCps' 58.0) :: forall f. Applicative f => Pitch f
b3' = Pitch $ pure (midiToCps' 59.0) :: forall f. Applicative f => Pitch f
c4' = Pitch $ pure (midiToCps' 60.0) :: forall f. Applicative f => Pitch f
cSharp4' = Pitch $ pure (midiToCps' 61.0) :: forall f. Applicative f => Pitch f
d4' = Pitch $ pure (midiToCps' 62.0) :: forall f. Applicative f => Pitch f
dSharp4' = Pitch $ pure (midiToCps' 63.0) :: forall f. Applicative f => Pitch f
e4' = Pitch $ pure (midiToCps' 64.0) :: forall f. Applicative f => Pitch f
f4' = Pitch $ pure (midiToCps' 65.0) :: forall f. Applicative f => Pitch f
fSharp4' = Pitch $ pure (midiToCps' 66.0) :: forall f. Applicative f => Pitch f
g4' = Pitch $ pure (midiToCps' 67.0) :: forall f. Applicative f => Pitch f
gSharp4' = Pitch $ pure (midiToCps' 68.0) :: forall f. Applicative f => Pitch f
a4' = Pitch $ pure (midiToCps' 69.0) :: forall f. Applicative f => Pitch f
aSharp4' = Pitch $ pure (midiToCps' 70.0) :: forall f. Applicative f => Pitch f
b4' = Pitch $ pure (midiToCps' 71.0) :: forall f. Applicative f => Pitch f
c5' = Pitch $ pure (midiToCps' 72.0) :: forall f. Applicative f => Pitch f
cSharp5' = Pitch $ pure (midiToCps' 73.0) :: forall f. Applicative f => Pitch f
d5' = Pitch $ pure (midiToCps' 74.0) :: forall f. Applicative f => Pitch f
dSharp5' = Pitch $ pure (midiToCps' 75.0) :: forall f. Applicative f => Pitch f
e5' = Pitch $ pure (midiToCps' 76.0) :: forall f. Applicative f => Pitch f
f5' = Pitch $ pure (midiToCps' 77.0) :: forall f. Applicative f => Pitch f
fSharp5' = Pitch $ pure (midiToCps' 78.0) :: forall f. Applicative f => Pitch f
g5' = Pitch $ pure (midiToCps' 79.0) :: forall f. Applicative f => Pitch f
gSharp5' = Pitch $ pure (midiToCps' 80.0) :: forall f. Applicative f => Pitch f
a5' = Pitch $ pure (midiToCps' 81.0) :: forall f. Applicative f => Pitch f
aSharp5' = Pitch $ pure (midiToCps' 82.0) :: forall f. Applicative f => Pitch f
b5' = Pitch $ pure (midiToCps' 83.0) :: forall f. Applicative f => Pitch f
c6' = Pitch $ pure (midiToCps' 84.0) :: forall f. Applicative f => Pitch f
cSharp6' = Pitch $ pure (midiToCps' 85.0) :: forall f. Applicative f => Pitch f
d6' = Pitch $ pure (midiToCps' 86.0) :: forall f. Applicative f => Pitch f
dSharp6' = Pitch $ pure (midiToCps' 87.0) :: forall f. Applicative f => Pitch f
e6' = Pitch $ pure (midiToCps' 88.0) :: forall f. Applicative f => Pitch f
f6' = Pitch $ pure (midiToCps' 89.0) :: forall f. Applicative f => Pitch f
fSharp6' = Pitch $ pure (midiToCps' 90.0) :: forall f. Applicative f => Pitch f
g6' = Pitch $ pure (midiToCps' 91.0) :: forall f. Applicative f => Pitch f
gSharp6' = Pitch $ pure (midiToCps' 92.0) :: forall f. Applicative f => Pitch f
a6' = Pitch $ pure (midiToCps' 93.0) :: forall f. Applicative f => Pitch f
aSharp6' = Pitch $ pure (midiToCps' 94.0) :: forall f. Applicative f => Pitch f
b6' = Pitch $ pure (midiToCps' 95.0) :: forall f. Applicative f => Pitch f
c7' = Pitch $ pure (midiToCps' 96.0) :: forall f. Applicative f => Pitch f
cSharp7' = Pitch $ pure (midiToCps' 97.0) :: forall f. Applicative f => Pitch f
d7' = Pitch $ pure (midiToCps' 98.0) :: forall f. Applicative f => Pitch f
dSharp7' = Pitch $ pure (midiToCps' 99.0) :: forall f. Applicative f => Pitch f
e7' = Pitch $ pure (midiToCps' 100.0) :: forall f. Applicative f => Pitch f
f7' = Pitch $ pure (midiToCps' 101.0) :: forall f. Applicative f => Pitch f
fSharp7' = Pitch $ pure (midiToCps' 102.0) :: forall f. Applicative f => Pitch f
g7' = Pitch $ pure (midiToCps' 103.0) :: forall f. Applicative f => Pitch f
gSharp7' = Pitch $ pure (midiToCps' 104.0) :: forall f. Applicative f => Pitch f
a7' = Pitch $ pure (midiToCps' 105.0) :: forall f. Applicative f => Pitch f
aSharp7' = Pitch $ pure (midiToCps' 106.0) :: forall f. Applicative f => Pitch f
b7' = Pitch $ pure (midiToCps' 107.0) :: forall f. Applicative f => Pitch f
dDoubleFlat0' = Pitch $ pure (midiToCps' 12.0) :: forall f. Applicative f => Pitch f
dFlat0' = Pitch $ pure (midiToCps' 13.0) :: forall f. Applicative f => Pitch f
cDoubleSharp0' = Pitch $ pure (midiToCps' 14.0) :: forall f. Applicative f => Pitch f
eDoubleFlat0' = Pitch $ pure (midiToCps' 14.0) :: forall f. Applicative f => Pitch f
eFlat0' = Pitch $ pure (midiToCps' 15.0) :: forall f. Applicative f => Pitch f
fDoubleFlat0' = Pitch $ pure (midiToCps' 15.0) :: forall f. Applicative f => Pitch f
dDoubleSharp0' = Pitch $ pure (midiToCps' 16.0) :: forall f. Applicative f => Pitch f
fFlat0' = Pitch $ pure (midiToCps' 16.0) :: forall f. Applicative f => Pitch f
eSharp0' = Pitch $ pure (midiToCps' 17.0) :: forall f. Applicative f => Pitch f
gDoubleFlat0' = Pitch $ pure (midiToCps' 17.0) :: forall f. Applicative f => Pitch f
eDoubleSharp0' = Pitch $ pure (midiToCps' 18.0) :: forall f. Applicative f => Pitch f
gFlat0' = Pitch $ pure (midiToCps' 18.0) :: forall f. Applicative f => Pitch f
fDoubleSharp0' = Pitch $ pure (midiToCps' 19.0) :: forall f. Applicative f => Pitch f
aDoubleFlat0' = Pitch $ pure (midiToCps' 19.0) :: forall f. Applicative f => Pitch f
aFlat0' = Pitch $ pure (midiToCps' 20.0) :: forall f. Applicative f => Pitch f
gDoubleSharp0' = Pitch $ pure (midiToCps' 21.0) :: forall f. Applicative f => Pitch f
bDoubleFlat0' = Pitch $ pure (midiToCps' 21.0) :: forall f. Applicative f => Pitch f
bFlat0' = Pitch $ pure (midiToCps' 22.0) :: forall f. Applicative f => Pitch f
cDoubleFlat1' = Pitch $ pure (midiToCps' 22.0) :: forall f. Applicative f => Pitch f
aDoubleSharp0' = Pitch $ pure (midiToCps' 23.0) :: forall f. Applicative f => Pitch f
cFlat1' = Pitch $ pure (midiToCps' 23.0) :: forall f. Applicative f => Pitch f
bSharp0' = Pitch $ pure (midiToCps' 24.0) :: forall f. Applicative f => Pitch f
dDoubleFlat1' = Pitch $ pure (midiToCps' 24.0) :: forall f. Applicative f => Pitch f
bDoubleSharp0' = Pitch $ pure (midiToCps' 25.0) :: forall f. Applicative f => Pitch f
dFlat1' = Pitch $ pure (midiToCps' 25.0) :: forall f. Applicative f => Pitch f
cDoubleSharp1' = Pitch $ pure (midiToCps' 26.0) :: forall f. Applicative f => Pitch f
eDoubleFlat1' = Pitch $ pure (midiToCps' 26.0) :: forall f. Applicative f => Pitch f
eFlat1' = Pitch $ pure (midiToCps' 27.0) :: forall f. Applicative f => Pitch f
fDoubleFlat1' = Pitch $ pure (midiToCps' 27.0) :: forall f. Applicative f => Pitch f
dDoubleSharp1' = Pitch $ pure (midiToCps' 28.0) :: forall f. Applicative f => Pitch f
fFlat1' = Pitch $ pure (midiToCps' 28.0) :: forall f. Applicative f => Pitch f
eSharp1' = Pitch $ pure (midiToCps' 29.0) :: forall f. Applicative f => Pitch f
gDoubleFlat1' = Pitch $ pure (midiToCps' 29.0) :: forall f. Applicative f => Pitch f
eDoubleSharp1' = Pitch $ pure (midiToCps' 30.0) :: forall f. Applicative f => Pitch f
gFlat1' = Pitch $ pure (midiToCps' 30.0) :: forall f. Applicative f => Pitch f
fDoubleSharp1' = Pitch $ pure (midiToCps' 31.0) :: forall f. Applicative f => Pitch f
aDoubleFlat1' = Pitch $ pure (midiToCps' 31.0) :: forall f. Applicative f => Pitch f
aFlat1' = Pitch $ pure (midiToCps' 32.0) :: forall f. Applicative f => Pitch f
gDoubleSharp1' = Pitch $ pure (midiToCps' 33.0) :: forall f. Applicative f => Pitch f
bDoubleFlat1' = Pitch $ pure (midiToCps' 33.0) :: forall f. Applicative f => Pitch f
bFlat1' = Pitch $ pure (midiToCps' 34.0) :: forall f. Applicative f => Pitch f
cDoubleFlat2' = Pitch $ pure (midiToCps' 34.0) :: forall f. Applicative f => Pitch f
aDoubleSharp1' = Pitch $ pure (midiToCps' 35.0) :: forall f. Applicative f => Pitch f
cFlat2' = Pitch $ pure (midiToCps' 35.0) :: forall f. Applicative f => Pitch f
bSharp1' = Pitch $ pure (midiToCps' 36.0) :: forall f. Applicative f => Pitch f
dDoubleFlat2' = Pitch $ pure (midiToCps' 36.0) :: forall f. Applicative f => Pitch f
bDoubleSharp1' = Pitch $ pure (midiToCps' 37.0) :: forall f. Applicative f => Pitch f
dFlat2' = Pitch $ pure (midiToCps' 37.0) :: forall f. Applicative f => Pitch f
cDoubleSharp2' = Pitch $ pure (midiToCps' 38.0) :: forall f. Applicative f => Pitch f
eDoubleFlat2' = Pitch $ pure (midiToCps' 38.0) :: forall f. Applicative f => Pitch f
eFlat2' = Pitch $ pure (midiToCps' 39.0) :: forall f. Applicative f => Pitch f
fDoubleFlat2' = Pitch $ pure (midiToCps' 39.0) :: forall f. Applicative f => Pitch f
dDoubleSharp2' = Pitch $ pure (midiToCps' 40.0) :: forall f. Applicative f => Pitch f
fFlat2' = Pitch $ pure (midiToCps' 40.0) :: forall f. Applicative f => Pitch f
eSharp2' = Pitch $ pure (midiToCps' 41.0) :: forall f. Applicative f => Pitch f
gDoubleFlat2' = Pitch $ pure (midiToCps' 41.0) :: forall f. Applicative f => Pitch f
eDoubleSharp2' = Pitch $ pure (midiToCps' 42.0) :: forall f. Applicative f => Pitch f
gFlat2' = Pitch $ pure (midiToCps' 42.0) :: forall f. Applicative f => Pitch f
fDoubleSharp2' = Pitch $ pure (midiToCps' 43.0) :: forall f. Applicative f => Pitch f
aDoubleFlat2' = Pitch $ pure (midiToCps' 43.0) :: forall f. Applicative f => Pitch f
aFlat2' = Pitch $ pure (midiToCps' 44.0) :: forall f. Applicative f => Pitch f
gDoubleSharp2' = Pitch $ pure (midiToCps' 45.0) :: forall f. Applicative f => Pitch f
bDoubleFlat2' = Pitch $ pure (midiToCps' 45.0) :: forall f. Applicative f => Pitch f
bFlat2' = Pitch $ pure (midiToCps' 46.0) :: forall f. Applicative f => Pitch f
cDoubleFlat3' = Pitch $ pure (midiToCps' 46.0) :: forall f. Applicative f => Pitch f
aDoubleSharp2' = Pitch $ pure (midiToCps' 47.0) :: forall f. Applicative f => Pitch f
cFlat3' = Pitch $ pure (midiToCps' 47.0) :: forall f. Applicative f => Pitch f
bSharp2' = Pitch $ pure (midiToCps' 48.0) :: forall f. Applicative f => Pitch f
dDoubleFlat3' = Pitch $ pure (midiToCps' 48.0) :: forall f. Applicative f => Pitch f
bDoubleSharp2' = Pitch $ pure (midiToCps' 49.0) :: forall f. Applicative f => Pitch f
dFlat3' = Pitch $ pure (midiToCps' 49.0) :: forall f. Applicative f => Pitch f
cDoubleSharp3' = Pitch $ pure (midiToCps' 50.0) :: forall f. Applicative f => Pitch f
eDoubleFlat3' = Pitch $ pure (midiToCps' 50.0) :: forall f. Applicative f => Pitch f
eFlat3' = Pitch $ pure (midiToCps' 51.0) :: forall f. Applicative f => Pitch f
fDoubleFlat3' = Pitch $ pure (midiToCps' 51.0) :: forall f. Applicative f => Pitch f
dDoubleSharp3' = Pitch $ pure (midiToCps' 52.0) :: forall f. Applicative f => Pitch f
fFlat3' = Pitch $ pure (midiToCps' 52.0) :: forall f. Applicative f => Pitch f
eSharp3' = Pitch $ pure (midiToCps' 53.0) :: forall f. Applicative f => Pitch f
gDoubleFlat3' = Pitch $ pure (midiToCps' 53.0) :: forall f. Applicative f => Pitch f
eDoubleSharp3' = Pitch $ pure (midiToCps' 54.0) :: forall f. Applicative f => Pitch f
gFlat3' = Pitch $ pure (midiToCps' 54.0) :: forall f. Applicative f => Pitch f
fDoubleSharp3' = Pitch $ pure (midiToCps' 55.0) :: forall f. Applicative f => Pitch f
aDoubleFlat3' = Pitch $ pure (midiToCps' 55.0) :: forall f. Applicative f => Pitch f
aFlat3' = Pitch $ pure (midiToCps' 56.0) :: forall f. Applicative f => Pitch f
gDoubleSharp3' = Pitch $ pure (midiToCps' 57.0) :: forall f. Applicative f => Pitch f
bDoubleFlat3' = Pitch $ pure (midiToCps' 57.0) :: forall f. Applicative f => Pitch f
bFlat3' = Pitch $ pure (midiToCps' 58.0) :: forall f. Applicative f => Pitch f
cDoubleFlat4' = Pitch $ pure (midiToCps' 58.0) :: forall f. Applicative f => Pitch f
aDoubleSharp3' = Pitch $ pure (midiToCps' 59.0) :: forall f. Applicative f => Pitch f
cFlat4' = Pitch $ pure (midiToCps' 59.0) :: forall f. Applicative f => Pitch f
bSharp3' = Pitch $ pure (midiToCps' 60.0) :: forall f. Applicative f => Pitch f
dDoubleFlat4' = Pitch $ pure (midiToCps' 60.0) :: forall f. Applicative f => Pitch f
bDoubleSharp3' = Pitch $ pure (midiToCps' 61.0) :: forall f. Applicative f => Pitch f
dFlat4' = Pitch $ pure (midiToCps' 61.0) :: forall f. Applicative f => Pitch f
cDoubleSharp4' = Pitch $ pure (midiToCps' 62.0) :: forall f. Applicative f => Pitch f
eDoubleFlat4' = Pitch $ pure (midiToCps' 62.0) :: forall f. Applicative f => Pitch f
eFlat4' = Pitch $ pure (midiToCps' 63.0) :: forall f. Applicative f => Pitch f
fDoubleFlat4' = Pitch $ pure (midiToCps' 63.0) :: forall f. Applicative f => Pitch f
dDoubleSharp4' = Pitch $ pure (midiToCps' 64.0) :: forall f. Applicative f => Pitch f
fFlat4' = Pitch $ pure (midiToCps' 64.0) :: forall f. Applicative f => Pitch f
eSharp4' = Pitch $ pure (midiToCps' 65.0) :: forall f. Applicative f => Pitch f
gDoubleFlat4' = Pitch $ pure (midiToCps' 65.0) :: forall f. Applicative f => Pitch f
eDoubleSharp4' = Pitch $ pure (midiToCps' 66.0) :: forall f. Applicative f => Pitch f
gFlat4' = Pitch $ pure (midiToCps' 66.0) :: forall f. Applicative f => Pitch f
fDoubleSharp4' = Pitch $ pure (midiToCps' 67.0) :: forall f. Applicative f => Pitch f
aDoubleFlat4' = Pitch $ pure (midiToCps' 67.0) :: forall f. Applicative f => Pitch f
aFlat4' = Pitch $ pure (midiToCps' 68.0) :: forall f. Applicative f => Pitch f
gDoubleSharp4' = Pitch $ pure (midiToCps' 69.0) :: forall f. Applicative f => Pitch f
bDoubleFlat4' = Pitch $ pure (midiToCps' 69.0) :: forall f. Applicative f => Pitch f
bFlat4' = Pitch $ pure (midiToCps' 70.0) :: forall f. Applicative f => Pitch f
cDoubleFlat5' = Pitch $ pure (midiToCps' 70.0) :: forall f. Applicative f => Pitch f
aDoubleSharp4' = Pitch $ pure (midiToCps' 71.0) :: forall f. Applicative f => Pitch f
cFlat5' = Pitch $ pure (midiToCps' 71.0) :: forall f. Applicative f => Pitch f
bSharp4' = Pitch $ pure (midiToCps' 72.0) :: forall f. Applicative f => Pitch f
dDoubleFlat5' = Pitch $ pure (midiToCps' 72.0) :: forall f. Applicative f => Pitch f
bDoubleSharp4' = Pitch $ pure (midiToCps' 73.0) :: forall f. Applicative f => Pitch f
dFlat5' = Pitch $ pure (midiToCps' 73.0) :: forall f. Applicative f => Pitch f
cDoubleSharp5' = Pitch $ pure (midiToCps' 74.0) :: forall f. Applicative f => Pitch f
eDoubleFlat5' = Pitch $ pure (midiToCps' 74.0) :: forall f. Applicative f => Pitch f
eFlat5' = Pitch $ pure (midiToCps' 75.0) :: forall f. Applicative f => Pitch f
fDoubleFlat5' = Pitch $ pure (midiToCps' 75.0) :: forall f. Applicative f => Pitch f
dDoubleSharp5' = Pitch $ pure (midiToCps' 76.0) :: forall f. Applicative f => Pitch f
fFlat5' = Pitch $ pure (midiToCps' 76.0) :: forall f. Applicative f => Pitch f
eSharp5' = Pitch $ pure (midiToCps' 77.0) :: forall f. Applicative f => Pitch f
gDoubleFlat5' = Pitch $ pure (midiToCps' 77.0) :: forall f. Applicative f => Pitch f
eDoubleSharp5' = Pitch $ pure (midiToCps' 78.0) :: forall f. Applicative f => Pitch f
gFlat5' = Pitch $ pure (midiToCps' 78.0) :: forall f. Applicative f => Pitch f
fDoubleSharp5' = Pitch $ pure (midiToCps' 79.0) :: forall f. Applicative f => Pitch f
aDoubleFlat5' = Pitch $ pure (midiToCps' 79.0) :: forall f. Applicative f => Pitch f
aFlat5' = Pitch $ pure (midiToCps' 80.0) :: forall f. Applicative f => Pitch f
gDoubleSharp5' = Pitch $ pure (midiToCps' 81.0) :: forall f. Applicative f => Pitch f
bDoubleFlat5' = Pitch $ pure (midiToCps' 81.0) :: forall f. Applicative f => Pitch f
bFlat5' = Pitch $ pure (midiToCps' 82.0) :: forall f. Applicative f => Pitch f
cDoubleFlat6' = Pitch $ pure (midiToCps' 82.0) :: forall f. Applicative f => Pitch f
aDoubleSharp5' = Pitch $ pure (midiToCps' 83.0) :: forall f. Applicative f => Pitch f
cFlat6' = Pitch $ pure (midiToCps' 83.0) :: forall f. Applicative f => Pitch f
bSharp5' = Pitch $ pure (midiToCps' 84.0) :: forall f. Applicative f => Pitch f
dDoubleFlat6' = Pitch $ pure (midiToCps' 84.0) :: forall f. Applicative f => Pitch f
bDoubleSharp5' = Pitch $ pure (midiToCps' 85.0) :: forall f. Applicative f => Pitch f
dFlat6' = Pitch $ pure (midiToCps' 85.0) :: forall f. Applicative f => Pitch f
cDoubleSharp6' = Pitch $ pure (midiToCps' 86.0) :: forall f. Applicative f => Pitch f
eDoubleFlat6' = Pitch $ pure (midiToCps' 86.0) :: forall f. Applicative f => Pitch f
eFlat6' = Pitch $ pure (midiToCps' 87.0) :: forall f. Applicative f => Pitch f
fDoubleFlat6' = Pitch $ pure (midiToCps' 87.0) :: forall f. Applicative f => Pitch f
dDoubleSharp6' = Pitch $ pure (midiToCps' 88.0) :: forall f. Applicative f => Pitch f
fFlat6' = Pitch $ pure (midiToCps' 88.0) :: forall f. Applicative f => Pitch f
eSharp6' = Pitch $ pure (midiToCps' 89.0) :: forall f. Applicative f => Pitch f
gDoubleFlat6' = Pitch $ pure (midiToCps' 89.0) :: forall f. Applicative f => Pitch f
eDoubleSharp6' = Pitch $ pure (midiToCps' 90.0) :: forall f. Applicative f => Pitch f
gFlat6' = Pitch $ pure (midiToCps' 90.0) :: forall f. Applicative f => Pitch f
fDoubleSharp6' = Pitch $ pure (midiToCps' 91.0) :: forall f. Applicative f => Pitch f
aDoubleFlat6' = Pitch $ pure (midiToCps' 91.0) :: forall f. Applicative f => Pitch f
aFlat6' = Pitch $ pure (midiToCps' 92.0) :: forall f. Applicative f => Pitch f
gDoubleSharp6' = Pitch $ pure (midiToCps' 93.0) :: forall f. Applicative f => Pitch f
bDoubleFlat6' = Pitch $ pure (midiToCps' 93.0) :: forall f. Applicative f => Pitch f
bFlat6' = Pitch $ pure (midiToCps' 94.0) :: forall f. Applicative f => Pitch f
cDoubleFlat7' = Pitch $ pure (midiToCps' 94.0) :: forall f. Applicative f => Pitch f
aDoubleSharp6' = Pitch $ pure (midiToCps' 95.0) :: forall f. Applicative f => Pitch f
cFlat7' = Pitch $ pure (midiToCps' 95.0) :: forall f. Applicative f => Pitch f
bSharp6' = Pitch $ pure (midiToCps' 96.0) :: forall f. Applicative f => Pitch f
dDoubleFlat7' = Pitch $ pure (midiToCps' 96.0) :: forall f. Applicative f => Pitch f
bDoubleSharp6' = Pitch $ pure (midiToCps' 97.0) :: forall f. Applicative f => Pitch f
dFlat7' = Pitch $ pure (midiToCps' 97.0) :: forall f. Applicative f => Pitch f
cDoubleSharp7' = Pitch $ pure (midiToCps' 98.0) :: forall f. Applicative f => Pitch f
eDoubleFlat7' = Pitch $ pure (midiToCps' 98.0) :: forall f. Applicative f => Pitch f
eFlat7' = Pitch $ pure (midiToCps' 99.0) :: forall f. Applicative f => Pitch f
fDoubleFlat7' = Pitch $ pure (midiToCps' 99.0) :: forall f. Applicative f => Pitch f
dDoubleSharp7' = Pitch $ pure (midiToCps' 100.0) :: forall f. Applicative f => Pitch f
fFlat7' = Pitch $ pure (midiToCps' 100.0) :: forall f. Applicative f => Pitch f
eSharp7' = Pitch $ pure (midiToCps' 101.0) :: forall f. Applicative f => Pitch f
gDoubleFlat7' = Pitch $ pure (midiToCps' 101.0) :: forall f. Applicative f => Pitch f
eDoubleSharp7' = Pitch $ pure (midiToCps' 102.0) :: forall f. Applicative f => Pitch f
gFlat7' = Pitch $ pure (midiToCps' 102.0) :: forall f. Applicative f => Pitch f
fDoubleSharp7' = Pitch $ pure (midiToCps' 103.0) :: forall f. Applicative f => Pitch f
aDoubleFlat7' = Pitch $ pure (midiToCps' 103.0) :: forall f. Applicative f => Pitch f
aFlat7' = Pitch $ pure (midiToCps' 104.0) :: forall f. Applicative f => Pitch f
gDoubleSharp7' = Pitch $ pure (midiToCps' 105.0) :: forall f. Applicative f => Pitch f
bDoubleFlat7' = Pitch $ pure (midiToCps' 105.0) :: forall f. Applicative f => Pitch f
bFlat7' = Pitch $ pure (midiToCps' 106.0) :: forall f. Applicative f => Pitch f
cDoubleFlat8' = Pitch $ pure (midiToCps' 106.0) :: forall f. Applicative f => Pitch f
aDoubleSharp7' = Pitch $ pure (midiToCps' 107.0) :: forall f. Applicative f => Pitch f
cFlat8' = Pitch $ pure (midiToCps' 107.0) :: forall f. Applicative f => Pitch f

middleC' = c4' :: forall f. Applicative f => Pitch f

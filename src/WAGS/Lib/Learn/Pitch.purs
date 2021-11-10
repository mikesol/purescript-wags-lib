module WAGS.Lib.Learn.Pitch where

import Prelude

import Data.Identity (Identity(..))
import Data.Int (round, toNumber)
import Data.Newtype (class Newtype)
import Math (log, pow)
import Safe.Coerce (coerce)
import Test.QuickCheck (arbitrary, class Arbitrary)
import WAGS.Lib.Learn.FofT (class FofT, toFofT)
import WAGS.Math (calcSlope)

type Time = Number
newtype Pitch f = Pitch (f Number)

derive instance newtypePitch :: Newtype (Pitch f) _
derive newtype instance eqPitch :: Eq (f Number) => Eq (Pitch f)
derive newtype instance ordPitch :: Ord (f Number) => Ord (Pitch f)
derive newtype instance boundedPitch :: Bounded (f Number) => Bounded (Pitch f)
derive newtype instance showPitch :: Show (f Number) => Show (Pitch f)
instance arbitraryPitchIdentity :: Applicative f => Arbitrary (Pitch f) where
  arbitrary = map (Pitch <<< pure <<< calcSlope 0.0 40.0 1.0 3000.0) arbitrary
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

at :: forall f. FofT f => Number -> Pitch f -> Pitch Identity
at time (Pitch p) = Pitch $ Identity (toFofT p time)

fuse :: forall f. FofT f => (Number -> Pitch f) -> Pitch ((->) Number)
fuse i = Pitch \t -> let Pitch p = i t in toFofT p t

fot :: forall f. FofT f => (Time -> Pitch f) -> Pitch ((->) Number)
fot f = Pitch \t -> let Pitch p = f t in toFofT p t

fot2 :: forall f. FofT f => (Time -> Number -> Number) -> Pitch f -> Pitch ((->) Number)
fot2 f (Pitch p) = Pitch \t -> f t (toFofT p t)

type BasePitch = Pitch Identity

unison = Pitch $ pure (midiToCps' 0.0) :: BasePitch
semitone = Pitch $ pure (midiToCps' 1.0) :: BasePitch
wholeTone = Pitch $ pure (midiToCps' 2.0) :: BasePitch
minorSecond = Pitch $ pure (midiToCps' 1.0) :: BasePitch
majorSecond = Pitch $ pure (midiToCps' 2.0) :: BasePitch
minorThird = Pitch $ pure (midiToCps' 3.0) :: BasePitch
majorThird = Pitch $ pure (midiToCps' 4.0) :: BasePitch
fourth = Pitch $ pure (midiToCps' 5.0) :: BasePitch
perfectFourth = Pitch $ pure (midiToCps' 5.0) :: BasePitch
augmentedFourth = Pitch $ pure (midiToCps' 6.0) :: BasePitch
diminishedFifth = Pitch $ pure (midiToCps' 6.0) :: BasePitch
fifth = Pitch $ pure (midiToCps' 7.0) :: BasePitch
perfectFifth = Pitch $ pure (midiToCps' 7.0) :: BasePitch
minorSixth = Pitch $ pure (midiToCps' 8.0) :: BasePitch
majorSixth = Pitch $ pure (midiToCps' 9.0) :: BasePitch
minorSeventh = Pitch $ pure (midiToCps' 10.0) :: BasePitch
majorSeventh = Pitch $ pure (midiToCps' 11.0) :: BasePitch
octave = Pitch $ pure (midiToCps' 12.0) :: BasePitch
perfectOctave = Pitch $ pure (midiToCps' 12.0) :: BasePitch

c0 = Pitch $ pure (midiToCps' 12.0) :: BasePitch
cSharp0 = Pitch $ pure (midiToCps' 13.0) :: BasePitch
d0 = Pitch $ pure (midiToCps' 14.0) :: BasePitch
dSharp0 = Pitch $ pure (midiToCps' 15.0) :: BasePitch
e0 = Pitch $ pure (midiToCps' 16.0) :: BasePitch
f0 = Pitch $ pure (midiToCps' 17.0) :: BasePitch
fSharp0 = Pitch $ pure (midiToCps' 18.0) :: BasePitch
g0 = Pitch $ pure (midiToCps' 19.0) :: BasePitch
gSharp0 = Pitch $ pure (midiToCps' 20.0) :: BasePitch
a0 = Pitch $ pure (midiToCps' 21.0) :: BasePitch
aSharp0 = Pitch $ pure (midiToCps' 22.0) :: BasePitch
b0 = Pitch $ pure (midiToCps' 23.0) :: BasePitch
c1 = Pitch $ pure (midiToCps' 24.0) :: BasePitch
cSharp1 = Pitch $ pure (midiToCps' 25.0) :: BasePitch
d1 = Pitch $ pure (midiToCps' 26.0) :: BasePitch
dSharp1 = Pitch $ pure (midiToCps' 27.0) :: BasePitch
e1 = Pitch $ pure (midiToCps' 28.0) :: BasePitch
f1 = Pitch $ pure (midiToCps' 29.0) :: BasePitch
fSharp1 = Pitch $ pure (midiToCps' 30.0) :: BasePitch
g1 = Pitch $ pure (midiToCps' 31.0) :: BasePitch
gSharp1 = Pitch $ pure (midiToCps' 32.0) :: BasePitch
a1 = Pitch $ pure (midiToCps' 33.0) :: BasePitch
aSharp1 = Pitch $ pure (midiToCps' 34.0) :: BasePitch
b1 = Pitch $ pure (midiToCps' 35.0) :: BasePitch
c2 = Pitch $ pure (midiToCps' 36.0) :: BasePitch
cSharp2 = Pitch $ pure (midiToCps' 37.0) :: BasePitch
d2 = Pitch $ pure (midiToCps' 38.0) :: BasePitch
dSharp2 = Pitch $ pure (midiToCps' 39.0) :: BasePitch
e2 = Pitch $ pure (midiToCps' 40.0) :: BasePitch
f2 = Pitch $ pure (midiToCps' 41.0) :: BasePitch
fSharp2 = Pitch $ pure (midiToCps' 42.0) :: BasePitch
g2 = Pitch $ pure (midiToCps' 43.0) :: BasePitch
gSharp2 = Pitch $ pure (midiToCps' 44.0) :: BasePitch
a2 = Pitch $ pure (midiToCps' 45.0) :: BasePitch
aSharp2 = Pitch $ pure (midiToCps' 46.0) :: BasePitch
b2 = Pitch $ pure (midiToCps' 47.0) :: BasePitch
c3 = Pitch $ pure (midiToCps' 48.0) :: BasePitch
cSharp3 = Pitch $ pure (midiToCps' 49.0) :: BasePitch
d3 = Pitch $ pure (midiToCps' 50.0) :: BasePitch
dSharp3 = Pitch $ pure (midiToCps' 51.0) :: BasePitch
e3 = Pitch $ pure (midiToCps' 52.0) :: BasePitch
f3 = Pitch $ pure (midiToCps' 53.0) :: BasePitch
fSharp3 = Pitch $ pure (midiToCps' 54.0) :: BasePitch
g3 = Pitch $ pure (midiToCps' 55.0) :: BasePitch
gSharp3 = Pitch $ pure (midiToCps' 56.0) :: BasePitch
a3 = Pitch $ pure (midiToCps' 57.0) :: BasePitch
aSharp3 = Pitch $ pure (midiToCps' 58.0) :: BasePitch
b3 = Pitch $ pure (midiToCps' 59.0) :: BasePitch
c4 = Pitch $ pure (midiToCps' 60.0) :: BasePitch
cSharp4 = Pitch $ pure (midiToCps' 61.0) :: BasePitch
d4 = Pitch $ pure (midiToCps' 62.0) :: BasePitch
dSharp4 = Pitch $ pure (midiToCps' 63.0) :: BasePitch
e4 = Pitch $ pure (midiToCps' 64.0) :: BasePitch
f4 = Pitch $ pure (midiToCps' 65.0) :: BasePitch
fSharp4 = Pitch $ pure (midiToCps' 66.0) :: BasePitch
g4 = Pitch $ pure (midiToCps' 67.0) :: BasePitch
gSharp4 = Pitch $ pure (midiToCps' 68.0) :: BasePitch
a4 = Pitch $ pure (midiToCps' 69.0) :: BasePitch
aSharp4 = Pitch $ pure (midiToCps' 70.0) :: BasePitch
b4 = Pitch $ pure (midiToCps' 71.0) :: BasePitch
c5 = Pitch $ pure (midiToCps' 72.0) :: BasePitch
cSharp5 = Pitch $ pure (midiToCps' 73.0) :: BasePitch
d5 = Pitch $ pure (midiToCps' 74.0) :: BasePitch
dSharp5 = Pitch $ pure (midiToCps' 75.0) :: BasePitch
e5 = Pitch $ pure (midiToCps' 76.0) :: BasePitch
f5 = Pitch $ pure (midiToCps' 77.0) :: BasePitch
fSharp5 = Pitch $ pure (midiToCps' 78.0) :: BasePitch
g5 = Pitch $ pure (midiToCps' 79.0) :: BasePitch
gSharp5 = Pitch $ pure (midiToCps' 80.0) :: BasePitch
a5 = Pitch $ pure (midiToCps' 81.0) :: BasePitch
aSharp5 = Pitch $ pure (midiToCps' 82.0) :: BasePitch
b5 = Pitch $ pure (midiToCps' 83.0) :: BasePitch
c6 = Pitch $ pure (midiToCps' 84.0) :: BasePitch
cSharp6 = Pitch $ pure (midiToCps' 85.0) :: BasePitch
d6 = Pitch $ pure (midiToCps' 86.0) :: BasePitch
dSharp6 = Pitch $ pure (midiToCps' 87.0) :: BasePitch
e6 = Pitch $ pure (midiToCps' 88.0) :: BasePitch
f6 = Pitch $ pure (midiToCps' 89.0) :: BasePitch
fSharp6 = Pitch $ pure (midiToCps' 90.0) :: BasePitch
g6 = Pitch $ pure (midiToCps' 91.0) :: BasePitch
gSharp6 = Pitch $ pure (midiToCps' 92.0) :: BasePitch
a6 = Pitch $ pure (midiToCps' 93.0) :: BasePitch
aSharp6 = Pitch $ pure (midiToCps' 94.0) :: BasePitch
b6 = Pitch $ pure (midiToCps' 95.0) :: BasePitch
c7 = Pitch $ pure (midiToCps' 96.0) :: BasePitch
cSharp7 = Pitch $ pure (midiToCps' 97.0) :: BasePitch
d7 = Pitch $ pure (midiToCps' 98.0) :: BasePitch
dSharp7 = Pitch $ pure (midiToCps' 99.0) :: BasePitch
e7 = Pitch $ pure (midiToCps' 100.0) :: BasePitch
f7 = Pitch $ pure (midiToCps' 101.0) :: BasePitch
fSharp7 = Pitch $ pure (midiToCps' 102.0) :: BasePitch
g7 = Pitch $ pure (midiToCps' 103.0) :: BasePitch
gSharp7 = Pitch $ pure (midiToCps' 104.0) :: BasePitch
a7 = Pitch $ pure (midiToCps' 105.0) :: BasePitch
aSharp7 = Pitch $ pure (midiToCps' 106.0) :: BasePitch
b7 = Pitch $ pure (midiToCps' 107.0) :: BasePitch
dDoubleFlat0 = Pitch $ pure (midiToCps' 12.0) :: BasePitch
dFlat0 = Pitch $ pure (midiToCps' 13.0) :: BasePitch
cDoubleSharp0 = Pitch $ pure (midiToCps' 14.0) :: BasePitch
eDoubleFlat0 = Pitch $ pure (midiToCps' 14.0) :: BasePitch
eFlat0 = Pitch $ pure (midiToCps' 15.0) :: BasePitch
fDoubleFlat0 = Pitch $ pure (midiToCps' 15.0) :: BasePitch
dDoubleSharp0 = Pitch $ pure (midiToCps' 16.0) :: BasePitch
fFlat0 = Pitch $ pure (midiToCps' 16.0) :: BasePitch
eSharp0 = Pitch $ pure (midiToCps' 17.0) :: BasePitch
gDoubleFlat0 = Pitch $ pure (midiToCps' 17.0) :: BasePitch
eDoubleSharp0 = Pitch $ pure (midiToCps' 18.0) :: BasePitch
gFlat0 = Pitch $ pure (midiToCps' 18.0) :: BasePitch
fDoubleSharp0 = Pitch $ pure (midiToCps' 19.0) :: BasePitch
aDoubleFlat0 = Pitch $ pure (midiToCps' 19.0) :: BasePitch
aFlat0 = Pitch $ pure (midiToCps' 20.0) :: BasePitch
gDoubleSharp0 = Pitch $ pure (midiToCps' 21.0) :: BasePitch
bDoubleFlat0 = Pitch $ pure (midiToCps' 21.0) :: BasePitch
bFlat0 = Pitch $ pure (midiToCps' 22.0) :: BasePitch
cDoubleFlat1 = Pitch $ pure (midiToCps' 22.0) :: BasePitch
aDoubleSharp0 = Pitch $ pure (midiToCps' 23.0) :: BasePitch
cFlat1 = Pitch $ pure (midiToCps' 23.0) :: BasePitch
bSharp0 = Pitch $ pure (midiToCps' 24.0) :: BasePitch
dDoubleFlat1 = Pitch $ pure (midiToCps' 24.0) :: BasePitch
bDoubleSharp0 = Pitch $ pure (midiToCps' 25.0) :: BasePitch
dFlat1 = Pitch $ pure (midiToCps' 25.0) :: BasePitch
cDoubleSharp1 = Pitch $ pure (midiToCps' 26.0) :: BasePitch
eDoubleFlat1 = Pitch $ pure (midiToCps' 26.0) :: BasePitch
eFlat1 = Pitch $ pure (midiToCps' 27.0) :: BasePitch
fDoubleFlat1 = Pitch $ pure (midiToCps' 27.0) :: BasePitch
dDoubleSharp1 = Pitch $ pure (midiToCps' 28.0) :: BasePitch
fFlat1 = Pitch $ pure (midiToCps' 28.0) :: BasePitch
eSharp1 = Pitch $ pure (midiToCps' 29.0) :: BasePitch
gDoubleFlat1 = Pitch $ pure (midiToCps' 29.0) :: BasePitch
eDoubleSharp1 = Pitch $ pure (midiToCps' 30.0) :: BasePitch
gFlat1 = Pitch $ pure (midiToCps' 30.0) :: BasePitch
fDoubleSharp1 = Pitch $ pure (midiToCps' 31.0) :: BasePitch
aDoubleFlat1 = Pitch $ pure (midiToCps' 31.0) :: BasePitch
aFlat1 = Pitch $ pure (midiToCps' 32.0) :: BasePitch
gDoubleSharp1 = Pitch $ pure (midiToCps' 33.0) :: BasePitch
bDoubleFlat1 = Pitch $ pure (midiToCps' 33.0) :: BasePitch
bFlat1 = Pitch $ pure (midiToCps' 34.0) :: BasePitch
cDoubleFlat2 = Pitch $ pure (midiToCps' 34.0) :: BasePitch
aDoubleSharp1 = Pitch $ pure (midiToCps' 35.0) :: BasePitch
cFlat2 = Pitch $ pure (midiToCps' 35.0) :: BasePitch
bSharp1 = Pitch $ pure (midiToCps' 36.0) :: BasePitch
dDoubleFlat2 = Pitch $ pure (midiToCps' 36.0) :: BasePitch
bDoubleSharp1 = Pitch $ pure (midiToCps' 37.0) :: BasePitch
dFlat2 = Pitch $ pure (midiToCps' 37.0) :: BasePitch
cDoubleSharp2 = Pitch $ pure (midiToCps' 38.0) :: BasePitch
eDoubleFlat2 = Pitch $ pure (midiToCps' 38.0) :: BasePitch
eFlat2 = Pitch $ pure (midiToCps' 39.0) :: BasePitch
fDoubleFlat2 = Pitch $ pure (midiToCps' 39.0) :: BasePitch
dDoubleSharp2 = Pitch $ pure (midiToCps' 40.0) :: BasePitch
fFlat2 = Pitch $ pure (midiToCps' 40.0) :: BasePitch
eSharp2 = Pitch $ pure (midiToCps' 41.0) :: BasePitch
gDoubleFlat2 = Pitch $ pure (midiToCps' 41.0) :: BasePitch
eDoubleSharp2 = Pitch $ pure (midiToCps' 42.0) :: BasePitch
gFlat2 = Pitch $ pure (midiToCps' 42.0) :: BasePitch
fDoubleSharp2 = Pitch $ pure (midiToCps' 43.0) :: BasePitch
aDoubleFlat2 = Pitch $ pure (midiToCps' 43.0) :: BasePitch
aFlat2 = Pitch $ pure (midiToCps' 44.0) :: BasePitch
gDoubleSharp2 = Pitch $ pure (midiToCps' 45.0) :: BasePitch
bDoubleFlat2 = Pitch $ pure (midiToCps' 45.0) :: BasePitch
bFlat2 = Pitch $ pure (midiToCps' 46.0) :: BasePitch
cDoubleFlat3 = Pitch $ pure (midiToCps' 46.0) :: BasePitch
aDoubleSharp2 = Pitch $ pure (midiToCps' 47.0) :: BasePitch
cFlat3 = Pitch $ pure (midiToCps' 47.0) :: BasePitch
bSharp2 = Pitch $ pure (midiToCps' 48.0) :: BasePitch
dDoubleFlat3 = Pitch $ pure (midiToCps' 48.0) :: BasePitch
bDoubleSharp2 = Pitch $ pure (midiToCps' 49.0) :: BasePitch
dFlat3 = Pitch $ pure (midiToCps' 49.0) :: BasePitch
cDoubleSharp3 = Pitch $ pure (midiToCps' 50.0) :: BasePitch
eDoubleFlat3 = Pitch $ pure (midiToCps' 50.0) :: BasePitch
eFlat3 = Pitch $ pure (midiToCps' 51.0) :: BasePitch
fDoubleFlat3 = Pitch $ pure (midiToCps' 51.0) :: BasePitch
dDoubleSharp3 = Pitch $ pure (midiToCps' 52.0) :: BasePitch
fFlat3 = Pitch $ pure (midiToCps' 52.0) :: BasePitch
eSharp3 = Pitch $ pure (midiToCps' 53.0) :: BasePitch
gDoubleFlat3 = Pitch $ pure (midiToCps' 53.0) :: BasePitch
eDoubleSharp3 = Pitch $ pure (midiToCps' 54.0) :: BasePitch
gFlat3 = Pitch $ pure (midiToCps' 54.0) :: BasePitch
fDoubleSharp3 = Pitch $ pure (midiToCps' 55.0) :: BasePitch
aDoubleFlat3 = Pitch $ pure (midiToCps' 55.0) :: BasePitch
aFlat3 = Pitch $ pure (midiToCps' 56.0) :: BasePitch
gDoubleSharp3 = Pitch $ pure (midiToCps' 57.0) :: BasePitch
bDoubleFlat3 = Pitch $ pure (midiToCps' 57.0) :: BasePitch
bFlat3 = Pitch $ pure (midiToCps' 58.0) :: BasePitch
cDoubleFlat4 = Pitch $ pure (midiToCps' 58.0) :: BasePitch
aDoubleSharp3 = Pitch $ pure (midiToCps' 59.0) :: BasePitch
cFlat4 = Pitch $ pure (midiToCps' 59.0) :: BasePitch
bSharp3 = Pitch $ pure (midiToCps' 60.0) :: BasePitch
dDoubleFlat4 = Pitch $ pure (midiToCps' 60.0) :: BasePitch
bDoubleSharp3 = Pitch $ pure (midiToCps' 61.0) :: BasePitch
dFlat4 = Pitch $ pure (midiToCps' 61.0) :: BasePitch
cDoubleSharp4 = Pitch $ pure (midiToCps' 62.0) :: BasePitch
eDoubleFlat4 = Pitch $ pure (midiToCps' 62.0) :: BasePitch
eFlat4 = Pitch $ pure (midiToCps' 63.0) :: BasePitch
fDoubleFlat4 = Pitch $ pure (midiToCps' 63.0) :: BasePitch
dDoubleSharp4 = Pitch $ pure (midiToCps' 64.0) :: BasePitch
fFlat4 = Pitch $ pure (midiToCps' 64.0) :: BasePitch
eSharp4 = Pitch $ pure (midiToCps' 65.0) :: BasePitch
gDoubleFlat4 = Pitch $ pure (midiToCps' 65.0) :: BasePitch
eDoubleSharp4 = Pitch $ pure (midiToCps' 66.0) :: BasePitch
gFlat4 = Pitch $ pure (midiToCps' 66.0) :: BasePitch
fDoubleSharp4 = Pitch $ pure (midiToCps' 67.0) :: BasePitch
aDoubleFlat4 = Pitch $ pure (midiToCps' 67.0) :: BasePitch
aFlat4 = Pitch $ pure (midiToCps' 68.0) :: BasePitch
gDoubleSharp4 = Pitch $ pure (midiToCps' 69.0) :: BasePitch
bDoubleFlat4 = Pitch $ pure (midiToCps' 69.0) :: BasePitch
bFlat4 = Pitch $ pure (midiToCps' 70.0) :: BasePitch
cDoubleFlat5 = Pitch $ pure (midiToCps' 70.0) :: BasePitch
aDoubleSharp4 = Pitch $ pure (midiToCps' 71.0) :: BasePitch
cFlat5 = Pitch $ pure (midiToCps' 71.0) :: BasePitch
bSharp4 = Pitch $ pure (midiToCps' 72.0) :: BasePitch
dDoubleFlat5 = Pitch $ pure (midiToCps' 72.0) :: BasePitch
bDoubleSharp4 = Pitch $ pure (midiToCps' 73.0) :: BasePitch
dFlat5 = Pitch $ pure (midiToCps' 73.0) :: BasePitch
cDoubleSharp5 = Pitch $ pure (midiToCps' 74.0) :: BasePitch
eDoubleFlat5 = Pitch $ pure (midiToCps' 74.0) :: BasePitch
eFlat5 = Pitch $ pure (midiToCps' 75.0) :: BasePitch
fDoubleFlat5 = Pitch $ pure (midiToCps' 75.0) :: BasePitch
dDoubleSharp5 = Pitch $ pure (midiToCps' 76.0) :: BasePitch
fFlat5 = Pitch $ pure (midiToCps' 76.0) :: BasePitch
eSharp5 = Pitch $ pure (midiToCps' 77.0) :: BasePitch
gDoubleFlat5 = Pitch $ pure (midiToCps' 77.0) :: BasePitch
eDoubleSharp5 = Pitch $ pure (midiToCps' 78.0) :: BasePitch
gFlat5 = Pitch $ pure (midiToCps' 78.0) :: BasePitch
fDoubleSharp5 = Pitch $ pure (midiToCps' 79.0) :: BasePitch
aDoubleFlat5 = Pitch $ pure (midiToCps' 79.0) :: BasePitch
aFlat5 = Pitch $ pure (midiToCps' 80.0) :: BasePitch
gDoubleSharp5 = Pitch $ pure (midiToCps' 81.0) :: BasePitch
bDoubleFlat5 = Pitch $ pure (midiToCps' 81.0) :: BasePitch
bFlat5 = Pitch $ pure (midiToCps' 82.0) :: BasePitch
cDoubleFlat6 = Pitch $ pure (midiToCps' 82.0) :: BasePitch
aDoubleSharp5 = Pitch $ pure (midiToCps' 83.0) :: BasePitch
cFlat6 = Pitch $ pure (midiToCps' 83.0) :: BasePitch
bSharp5 = Pitch $ pure (midiToCps' 84.0) :: BasePitch
dDoubleFlat6 = Pitch $ pure (midiToCps' 84.0) :: BasePitch
bDoubleSharp5 = Pitch $ pure (midiToCps' 85.0) :: BasePitch
dFlat6 = Pitch $ pure (midiToCps' 85.0) :: BasePitch
cDoubleSharp6 = Pitch $ pure (midiToCps' 86.0) :: BasePitch
eDoubleFlat6 = Pitch $ pure (midiToCps' 86.0) :: BasePitch
eFlat6 = Pitch $ pure (midiToCps' 87.0) :: BasePitch
fDoubleFlat6 = Pitch $ pure (midiToCps' 87.0) :: BasePitch
dDoubleSharp6 = Pitch $ pure (midiToCps' 88.0) :: BasePitch
fFlat6 = Pitch $ pure (midiToCps' 88.0) :: BasePitch
eSharp6 = Pitch $ pure (midiToCps' 89.0) :: BasePitch
gDoubleFlat6 = Pitch $ pure (midiToCps' 89.0) :: BasePitch
eDoubleSharp6 = Pitch $ pure (midiToCps' 90.0) :: BasePitch
gFlat6 = Pitch $ pure (midiToCps' 90.0) :: BasePitch
fDoubleSharp6 = Pitch $ pure (midiToCps' 91.0) :: BasePitch
aDoubleFlat6 = Pitch $ pure (midiToCps' 91.0) :: BasePitch
aFlat6 = Pitch $ pure (midiToCps' 92.0) :: BasePitch
gDoubleSharp6 = Pitch $ pure (midiToCps' 93.0) :: BasePitch
bDoubleFlat6 = Pitch $ pure (midiToCps' 93.0) :: BasePitch
bFlat6 = Pitch $ pure (midiToCps' 94.0) :: BasePitch
cDoubleFlat7 = Pitch $ pure (midiToCps' 94.0) :: BasePitch
aDoubleSharp6 = Pitch $ pure (midiToCps' 95.0) :: BasePitch
cFlat7 = Pitch $ pure (midiToCps' 95.0) :: BasePitch
bSharp6 = Pitch $ pure (midiToCps' 96.0) :: BasePitch
dDoubleFlat7 = Pitch $ pure (midiToCps' 96.0) :: BasePitch
bDoubleSharp6 = Pitch $ pure (midiToCps' 97.0) :: BasePitch
dFlat7 = Pitch $ pure (midiToCps' 97.0) :: BasePitch
cDoubleSharp7 = Pitch $ pure (midiToCps' 98.0) :: BasePitch
eDoubleFlat7 = Pitch $ pure (midiToCps' 98.0) :: BasePitch
eFlat7 = Pitch $ pure (midiToCps' 99.0) :: BasePitch
fDoubleFlat7 = Pitch $ pure (midiToCps' 99.0) :: BasePitch
dDoubleSharp7 = Pitch $ pure (midiToCps' 100.0) :: BasePitch
fFlat7 = Pitch $ pure (midiToCps' 100.0) :: BasePitch
eSharp7 = Pitch $ pure (midiToCps' 101.0) :: BasePitch
gDoubleFlat7 = Pitch $ pure (midiToCps' 101.0) :: BasePitch
eDoubleSharp7 = Pitch $ pure (midiToCps' 102.0) :: BasePitch
gFlat7 = Pitch $ pure (midiToCps' 102.0) :: BasePitch
fDoubleSharp7 = Pitch $ pure (midiToCps' 103.0) :: BasePitch
aDoubleFlat7 = Pitch $ pure (midiToCps' 103.0) :: BasePitch
aFlat7 = Pitch $ pure (midiToCps' 104.0) :: BasePitch
gDoubleSharp7 = Pitch $ pure (midiToCps' 105.0) :: BasePitch
bDoubleFlat7 = Pitch $ pure (midiToCps' 105.0) :: BasePitch
bFlat7 = Pitch $ pure (midiToCps' 106.0) :: BasePitch
cDoubleFlat8 = Pitch $ pure (midiToCps' 106.0) :: BasePitch
aDoubleSharp7 = Pitch $ pure (midiToCps' 107.0) :: BasePitch
cFlat8 = Pitch $ pure (midiToCps' 107.0) :: BasePitch

middleC = c4 :: BasePitch


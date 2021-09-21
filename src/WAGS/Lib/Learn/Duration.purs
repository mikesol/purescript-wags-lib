module WAGS.Lib.Learn.Duration where

import Prelude

import Data.Newtype (class Newtype, over)

class Dot a where
  dot :: a -> a

instance dotDuration :: Dot Duration where
  dot = over Duration (mul 1.5)

instance dotRest :: Dot Rest where
  dot = over Rest (mul 1.5)

newtype Duration = Duration Number

derive instance newtypeDuration :: Newtype Duration _
derive newtype instance boundedDuration :: Bounded Duration
derive newtype instance divisionringDuration :: DivisionRing Duration
derive newtype instance eqDuration :: Eq Duration
derive newtype instance commutativeRingDuration :: CommutativeRing Duration
derive newtype instance euclideanRingDuration :: EuclideanRing Duration
derive newtype instance ordDuration :: Ord Duration
derive newtype instance ringDuration :: Ring Duration
derive newtype instance semiringDuration :: Semiring Duration
derive newtype instance showDuration :: Show Duration

hemidemisemiquaver = Duration 0.0625 :: Duration
demisemiquaver = Duration 0.125 :: Duration
semiquaver = Duration 0.25 :: Duration
quaver = Duration 0.5 :: Duration
crochet = Duration 1.0 :: Duration
minim = Duration 2.0 :: Duration
semibreve = Duration 4.0 :: Duration
breve = Duration 8.0 :: Duration

dottedHemidemisemiquaver = dot hemidemisemiquaver :: Duration
dottedDemisemiquaver = dot demisemiquaver :: Duration
dottedSemiquaver = dot semiquaver :: Duration
dottedQuaver = dot quaver :: Duration
dottedCrochet = dot crochet :: Duration
dottedMinim = dot minim :: Duration
dottedSemibreve = dot semibreve :: Duration
dottedBreve = dot breve :: Duration

newtype Rest = Rest Number

derive instance newtypeRest :: Newtype Rest _
derive newtype instance boundedRest :: Bounded Rest
derive newtype instance divisionringRest :: DivisionRing Rest
derive newtype instance eqRest :: Eq Rest
derive newtype instance commutativeRingRest :: CommutativeRing Rest
derive newtype instance euclideanRingRest :: EuclideanRing Rest
derive newtype instance ordRest :: Ord Rest
derive newtype instance ringRest :: Ring Rest
derive newtype instance semiringRest :: Semiring Rest
derive newtype instance showRest :: Show Rest

hemidemisemiquaverRest = Rest 0.0625 :: Rest
demisemiquaverRest = Rest 0.125 :: Rest
semiquaverRest = Rest 0.25 :: Rest
quaverRest = Rest 0.5 :: Rest
crochetRest = Rest 1.0 :: Rest
minimRest = Rest 2.0 :: Rest
semibreveRest = Rest 4.0 :: Rest
breveRest = Rest 8.0 :: Rest

dottedHemidemisemiquaverRest = dot hemidemisemiquaverRest :: Rest
dottedDemisemiquaverRest = dot demisemiquaverRest :: Rest
dottedSemiquaverRest = dot semiquaverRest :: Rest
dottedQuaverRest = dot quaverRest :: Rest
dottedCrochetRest = dot crochetRest :: Rest
dottedMinimRest = dot minimRest :: Rest
dottedSemibreveRest = dot semibreveRest :: Rest
dottedBreveRest = dot breveRest :: Rest



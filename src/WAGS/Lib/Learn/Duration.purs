module WAGS.Lib.Learn.Duration where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (class Newtype)

class Dot a where
  dot :: a -> a

instance dotDuration :: Applicative f => Dot (Duration f) where
  dot = mul (div (one + one + one) (one + one))

instance dotRest :: Applicative f => Dot (Rest f) where
  dot =  mul (div (one + one + one) (one + one))

newtype Duration f = Duration (f Number)

derive instance newtypeDuration :: Newtype (Duration f) _
derive newtype instance eqDuration :: Eq (f Number) => Eq (Duration f)
derive newtype instance ordDuration :: Ord (f Number) => Ord (Duration f)
derive newtype instance boundedDuration :: Bounded (f Number) => Bounded (Duration f)
derive newtype instance showDuration :: Show (f Number) => Show (Duration f)
instance commutativeRingDuration :: Applicative f => CommutativeRing (Duration f)
instance euclideanRingDuration :: Applicative f => EuclideanRing (Duration f) where
  degree _ = 1
  div (Duration a) (Duration b) = Duration ((div <$> (a) <*> (b)))
  mod (Duration a) (Duration b) = Duration ((mod <$> (a) <*> (b)))
instance ringDuration :: Applicative f => Ring (Duration f) where
  sub (Duration a) (Duration b) = Duration ((sub <$> (a) <*> (b)))
instance semiringDuration :: Applicative f => Semiring (Duration f) where
  zero = Duration (pure zero)
  one = Duration (pure one)
  add (Duration a) (Duration b) = Duration ((add <$> (a) <*> (b)))
  mul (Duration a) (Duration b) = Duration ((mul <$> (a) <*> (b)))

hemidemisemiquaver = Duration $ Identity $ 0.0625 :: Duration Identity
demisemiquaver = Duration $ Identity $ 0.125 :: Duration Identity
semiquaver = Duration $ Identity $ 0.25 :: Duration Identity
quaver = Duration $ Identity $ 0.5 :: Duration Identity
crochet = Duration $ Identity $ 1.0 :: Duration Identity
minim = Duration $ Identity $ 2.0 :: Duration Identity
semibreve = Duration $ Identity $ 4.0 :: Duration Identity
breve = Duration $ Identity $ 8.0 :: Duration Identity

dottedHemidemisemiquaver = dot hemidemisemiquaver :: Duration Identity
dottedDemisemiquaver = dot demisemiquaver :: Duration Identity
dottedSemiquaver = dot semiquaver :: Duration Identity
dottedQuaver = dot quaver :: Duration Identity
dottedCrochet = dot crochet :: Duration Identity
dottedMinim = dot minim :: Duration Identity
dottedSemibreve = dot semibreve :: Duration Identity
dottedBreve = dot breve :: Duration Identity

newtype Rest f = Rest (f Number)

derive instance newtypeRest :: Newtype (Rest f) _
derive newtype instance eqRest :: Eq (f Number) => Eq (Rest f)
derive newtype instance ordRest :: Ord (f Number) => Ord (Rest f)
derive newtype instance boundedRest :: Bounded (f Number) => Bounded (Rest f)
derive newtype instance showRest :: Show (f Number) => Show (Rest f)
instance commutativeRingRest :: Applicative f => CommutativeRing (Rest f)
instance euclideanRingRest :: Applicative f => EuclideanRing (Rest f) where
  degree _ = 1
  div (Rest a) (Rest b) = Rest ((div <$> (a) <*> (b)))
  mod (Rest a) (Rest b) = Rest ((mod <$> (a) <*> (b)))
instance ringRest :: Applicative f => Ring (Rest f) where
  sub (Rest a) (Rest b) = Rest ((sub <$> (a) <*> (b)))
instance semiringRest :: Applicative f => Semiring (Rest f) where
  zero = Rest (pure zero)
  one = Rest (pure one)
  add (Rest a) (Rest b) = Rest ((add <$> (a) <*> (b)))
  mul (Rest a) (Rest b) = Rest ((mul <$> (a) <*> (b)))

hemidemisemiquaverRest = Rest $ Identity 0.0625 :: Rest Identity
demisemiquaverRest = Rest $ Identity 0.125 :: Rest Identity
semiquaverRest = Rest $ Identity 0.25 :: Rest Identity
quaverRest = Rest $ Identity 0.5 :: Rest Identity
crochetRest = Rest $ Identity 1.0 :: Rest Identity
minimRest = Rest $ Identity 2.0 :: Rest Identity
semibreveRest = Rest $ Identity 4.0 :: Rest Identity
breveRest = Rest $ Identity 8.0 :: Rest Identity

dottedHemidemisemiquaverRest = dot hemidemisemiquaverRest :: Rest Identity
dottedDemisemiquaverRest = dot demisemiquaverRest :: Rest Identity
dottedSemiquaverRest = dot semiquaverRest :: Rest Identity
dottedQuaverRest = dot quaverRest :: Rest Identity
dottedCrochetRest = dot crochetRest :: Rest Identity
dottedMinimRest = dot minimRest :: Rest Identity
dottedSemibreveRest = dot semibreveRest :: Rest Identity
dottedBreveRest = dot breveRest :: Rest Identity



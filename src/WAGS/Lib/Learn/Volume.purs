module WAGS.Lib.Learn.Volume where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (class Newtype)
import Math (pow)

newtype Volume f = Volume (f Number)

derive instance newtypeVolume :: Newtype (Volume f) _
derive newtype instance eqVolume :: Eq (f Number) => Eq (Volume f)
derive newtype instance ordVolume :: Ord (f Number) => Ord (Volume f)
derive newtype instance boundedVolume :: Bounded (f Number) => Bounded (Volume f)
derive newtype instance showVolume :: Show (f Number) => Show (Volume f)
instance commutativeRingVolume :: Applicative f => CommutativeRing (Volume f)
instance euclideanRingVolume :: Applicative f => EuclideanRing (Volume f) where
  degree _ = 1
  div (Volume a) (Volume b) = Volume ((div <$> (a) <*> (b)))
  mod (Volume a) (Volume b) = Volume ((mod <$> (a) <*> (b)))
instance ringVolume :: Applicative f => Ring (Volume f) where
  sub (Volume a) (Volume b) = Volume ((sub <$> (a) <*> (b)))
instance semiringVolume :: Applicative f => Semiring (Volume f) where
  zero = Volume (pure zero)
  one = Volume (pure one)
  add (Volume a) (Volume b) = Volume ((add <$> (a) <*> (b)))
  mul (Volume a) (Volume b) = Volume ((mul <$> (a) <*> (b)))

fortississimo = Volume $ Identity $ 0.5 `pow` 0.0 :: Volume Identity
fortissimo = Volume $ Identity $ 0.5 `pow` 0.5 :: Volume Identity
forte = Volume $ Identity $ 0.5 `pow` 1.0 :: Volume Identity
mezzoForte = Volume $ Identity $ 0.5 `pow` 1.5 :: Volume Identity
mezzoPiano = Volume $ Identity $ 0.5 `pow` 2.0 :: Volume Identity
piano = Volume $ Identity $ 0.5 `pow` 3.0 :: Volume Identity
pianissimo = Volume $ Identity $ 0.5 `pow` 5.0 :: Volume Identity
pianississimo = Volume $ Identity $ 0.5 `pow` 8.0 :: Volume Identity

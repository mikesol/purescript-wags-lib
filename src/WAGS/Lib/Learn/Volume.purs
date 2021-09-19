module WAGS.Lib.Learn.Volume where

import Prelude

import Data.Newtype (class Newtype)
import Math (pow)

newtype Volume = Volume Number

derive instance newtypeVolume :: Newtype Volume _
derive newtype instance boundedVolume :: Bounded Volume
derive newtype instance divisionringVolume :: DivisionRing Volume
derive newtype instance eqVolume :: Eq Volume
derive newtype instance commutativeRingVolume :: CommutativeRing Volume
derive newtype instance euclideanRingVolume :: EuclideanRing Volume
derive newtype instance ordVolume :: Ord Volume
derive newtype instance ringVolume :: Ring Volume
derive newtype instance semiringVolume :: Semiring Volume
derive newtype instance showVolume :: Show Volume

fortississimo = Volume $ 0.5 `pow` 0.0 :: Volume
fortissimo = Volume $ 0.5 `pow` 0.5 :: Volume
forte = Volume $ 0.5 `pow` 1.0 :: Volume
mezzoForte = Volume $ 0.5 `pow` 1.5 :: Volume
mezzoPiano = Volume $ 0.5 `pow` 2.0 :: Volume
piano = Volume $ 0.5 `pow` 3.0 :: Volume
pianissimo = Volume $ 0.5 `pow` 5.0 :: Volume
pianississimo = Volume $ 0.5 `pow` 8.0 :: Volume

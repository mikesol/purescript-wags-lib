module App.Lib where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Typelevel.Num (class Lt, class Nat)
import Data.Vec as V

type RateInfo =
  { starting :: Boolean -- is the sector starting?
  , clockTime :: Number -- how much time has passed
  , bufferTime :: Number -- how much of the buffer has played before now
  , lastRate :: Maybe Number -- the previous rate if there was one
  , sector :: Int -- the current sector
  }

ix :: forall i s a. Nat i => Lt i s => i -> V.Vec s a -> a
ix i v = V.index v i

type Order (s :: Type) (a :: Type) = NonEmpty List (V.Vec s a -> a)

usingNSectors :: forall s a. Nat s => Order s a -> Order s a
usingNSectors = identity
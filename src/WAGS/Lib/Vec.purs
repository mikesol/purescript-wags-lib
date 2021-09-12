module WAGS.Lib.Vec where

import Prelude

import Data.Array as A
import Data.Maybe (fromJust)
import Data.Vec as V
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

indexMod :: forall n a. V.Vec n a -> Int -> a
indexMod v i = unsafePartial $ fromJust $ A.index a (i `mod` A.length a)
  where
  a :: Array a
  a = unsafeCoerce v
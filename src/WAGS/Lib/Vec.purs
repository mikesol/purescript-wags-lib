module WAGS.Lib.Vec where

import Prelude

import Data.Array as A
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Nat, class Succ, D0)
import Data.Vec as V
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

indexMod :: forall n a. V.Vec n a -> Int -> a
indexMod v i = unsafePartial $ fromJust $ A.index a (i `mod` A.length a)
  where
  a :: Array a
  a = unsafeCoerce v

class Nat n <= FoldV n where
  foldV :: forall r a b. (Tuple r a -> Tuple r b) -> r -> V.Vec n a -> Tuple r (V.Vec n b)

instance foldVD0 :: FoldV D0 where
  foldV f r = Tuple r <<< map (Tuple r >>> f >>> snd)
else instance foldVPred :: (Succ nMinus1 n, FoldV nMinus1) => FoldV n where
  foldV f r v = Tuple (fst rest) (V.cons (snd res) (snd rest))
    where
    { head, tail } = V.uncons v
    res = f (Tuple r head)
    rest = foldV f (fst res) tail
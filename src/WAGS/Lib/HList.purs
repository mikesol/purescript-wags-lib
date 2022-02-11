module WAGS.Lib.HList where

import Prelude

import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, folding, foldingWithIndex, hfoldl, hfoldlWithIndex)

data Peano'
foreign import data S :: Peano' -> Peano'
foreign import data Z :: Peano'

data Peano (p :: Peano') = Peano

class KnownPeano p where
  reflectPeano :: Peano p -> Int

instance knownPeanoZ :: KnownPeano Z where
  reflectPeano _ = 0

instance knownPeanoS :: KnownPeano p => KnownPeano (S p) where
  reflectPeano _ = 1 + reflectPeano (Peano :: Peano p)

data HCons a b = HCons a b
data HNil = HNil

infixr 8 type HCons as :
infixr 8 HCons as :

newtype Counting (p :: Peano') a = Counting a

instance hfoldlWithIndexCounting_nil :: HFoldlWithIndex f x (Counting p HNil) x where
  hfoldlWithIndex _ x _ = x

instance hfoldlWithIndexCounting_go ::
  ( FoldingWithIndex f (Peano p) x a y
  , HFoldlWithIndex f y (Counting (S p) b) z
  ) =>
  HFoldlWithIndex f x (Counting p (HCons a b)) z
  where
  hfoldlWithIndex f x (Counting (HCons a b)) = z
    where
    y = foldingWithIndex f (Peano :: Peano p) x a
    z = hfoldlWithIndex f y (Counting b :: Counting (S p) b)

instance hfoldlWithIndexHNil :: HFoldlWithIndex f x HNil x where
  hfoldlWithIndex _ x _ = x

instance hfoldlWithIndexHCons ::
  ( FoldingWithIndex f (Peano Z) x a y
  , HFoldlWithIndex f y (Counting (S Z) rest) z
  ) =>
  HFoldlWithIndex f x (HCons a rest) z
  where
  hfoldlWithIndex f x (HCons a rest) = z
    where
    y = foldingWithIndex f (Peano :: Peano Z) x a
    z = hfoldlWithIndex f y (Counting rest :: Counting (S Z) rest)

instance hfoldlHNil :: HFoldl f x HNil x where
  hfoldl _ x _ = x
else
instance hfoldlHCons_one ::
  Folding f x a z =>
  HFoldl f x (HCons a HNil) z
  where
  hfoldl f x (HCons a _) =
    folding f x a
else
instance hfoldlHCons_many ::
  ( Folding f x a y
  , HFoldl f y (HCons b c) z
  ) =>
  HFoldl f x (HCons a (HCons b c)) z
  where
  hfoldl f x (HCons a rest) =
    hfoldl f (folding f x a) rest
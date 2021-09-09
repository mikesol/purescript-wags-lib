module WAGS.Lib.Cofree where

import Prelude hiding (Ordering(..))

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, deferCofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Typelevel.Num (class Nat)
import Data.Tuple.Nested ((/\))
import Data.Vec as V
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)

--- heads and tails needs recursion down records to work
--- otherwise namespacing is impossible
data Tailz
  = Tailz

class Tailable a b | a -> b where
  tails :: a -> b

instance tailableRow :: HMap Tailz { | ii } { | oo } => Tailable { | ii } { | oo } where
  tails = hmap Tailz
else instance tailableVec :: (Nat n, Tailable i o) => Tailable (V.Vec n i) (V.Vec n o) where
  tails = map tails
else instance tailableCf :: ComonadCofree f w => Tailable (w a) (f (w a)) where
  tails = unwrapCofree

instance tailzMapping ::
  Tailable i o =>
  Mapping Tailz i o where
  mapping Tailz = tails

data Headz
  = Headz

class Headable a b | a -> b where
  heads :: a -> b

instance headableRow :: HMap Headz { | ii } { | oo } => Headable { | ii } { | oo } where
  heads = hmap Headz
else instance headableVec :: (Nat n, Headable i o) => Headable (V.Vec n i) (V.Vec n o) where
  heads = map heads
else instance headableCf :: Comonad w => Headable (w a) a where
  heads = extract

instance headzMapping ::
  Headable i o =>
  Mapping Headz i o where
  mapping Headz = heads

convolve
  :: forall f a g b h c
   . Functor f
  => Functor g
  => (a -> b -> c)
  -> (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> Cofree f a
  -> Cofree g b
  -> Cofree h c
convolve f1 f2 c1 c2 = f1 (extract c1) (extract c2) :< convolveComonadCofree f1 f2 (unwrapCofree c1) (unwrapCofree c2)

convolveComonadCofree
  :: forall f a g b h c
   . Functor f
  => Functor g
  => (a -> b -> c)
  -> (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> f (Cofree f a)
  -> g (Cofree g b)
  -> h (Cofree h c)
convolveComonadCofree f1 f2 = f2 (convolve f1 f2)

convolveComonadCofreeChooseB
  :: forall f a g b h
   . Functor f
  => Functor g
  => (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> f (Cofree f a)
  -> g (Cofree g b)
  -> h (Cofree h b)
convolveComonadCofreeChooseB = convolveComonadCofree (const identity)

composeComonadCofree
  :: forall f a b
   . Functor f
  => f (Cofree f a)
  -> (a -> Cofree ((->) a) b)
  -> f (Cofree f b)
composeComonadCofree = convolveComonadCofreeChooseB (\cont e b -> map (cont <*> b <<< extract) e)

deferConvolve
  :: forall f a g b h c
   . Functor f
  => Functor g
  => (a -> b -> c)
  -> (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> Cofree f a
  -> Cofree g b
  -> Cofree h c
deferConvolve f1 f2 c1 c2 =
  deferCofree
    \_ -> f1 (extract c1) (extract c2) /\ deferConvolveComonadCofree f1 f2 (unwrapCofree c1) (unwrapCofree c2)

deferConvolveComonadCofree
  :: forall f a g b h c
   . Functor f
  => Functor g
  => (a -> b -> c)
  -> (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> f (Cofree f a)
  -> g (Cofree g b)
  -> h (Cofree h c)
deferConvolveComonadCofree f1 f2 = f2 (deferConvolve f1 f2)

deferConvolveComonadCofreeChooseB
  :: forall f a g b h
   . Functor f
  => Functor g
  => (forall z. (Cofree f a -> Cofree g b -> z) -> f (Cofree f a) -> g (Cofree g b) -> h z)
  -> f (Cofree f a)
  -> g (Cofree g b)
  -> h (Cofree h b)
deferConvolveComonadCofreeChooseB = deferConvolveComonadCofree (const identity)

deferComposeComonadCofree
  :: forall f a b
   . Functor f
  => f (Cofree f a)
  -> (a -> Cofree ((->) a) b)
  -> f (Cofree f b)
deferComposeComonadCofree = deferConvolveComonadCofreeChooseB (\cont e b -> map (cont <*> b <<< extract) e)

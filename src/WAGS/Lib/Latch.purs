module WAGS.Lib.Latch where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Graph.Parameter (AudioParameter_(..))
import WAGS.Lib.Cofree (class Actualize)

newtype MakeLatchAP v a
  = MakeLatchAP (AudioParameter_ v -> a)

derive instance newtypeMakeLatchAP :: Newtype (MakeLatchAP v a) _

derive instance functorMakeLatchAP :: Functor (MakeLatchAP v)

derive newtype instance semigroupMakeLatchAP :: Semigroup a => Semigroup (MakeLatchAP v a)

type LatchAP v
  = Maybe (AudioParameter_ v)

newtype CfLatchAP f a
  = CfLatchAP (Cofree f a)

derive instance newtypeCfLatchAP :: Newtype (CfLatchAP (MakeLatchAP v) (LatchAP v)) _

derive instance functorCfLatchAP :: Functor (CfLatchAP (MakeLatchAP v))

derive newtype instance extendCfLatchAP :: Extend (CfLatchAP (MakeLatchAP v))

derive newtype instance comonadCfLatchAP :: Comonad (CfLatchAP (MakeLatchAP v))

derive newtype instance comonadCofreeCfLatchAP :: ComonadCofree (MakeLatchAP v) (CfLatchAP (MakeLatchAP v))

type ALatchAP v
  = MakeLatchAP v (CfLatchAP (MakeLatchAP v) (LatchAP v))

cmpAP :: forall v. Eq v => Maybe (AudioParameter_ v) -> Maybe (AudioParameter_ v) -> Boolean
cmpAP Nothing Nothing = true

cmpAP (Just _) (Nothing) = false

cmpAP (Nothing) (Just _) = false

cmpAP (Just (AudioParameter { param: p0 })) (Just (AudioParameter { param: p1 })) = p0 == p1

makeLatchAP :: forall v. Eq v => ALatchAP v
makeLatchAP = wrap (\p -> go Nothing p)
  where
  go :: LatchAP v -> AudioParameter_ v -> CfLatchAP (MakeLatchAP v) (LatchAP v)
  go old new = wrap ((if old `cmpAP` Just new then Nothing else Just new) :< map unwrap (wrap (go (Just new))))

makeLatchAPWithoutInitialBlip :: forall v. Eq v => ALatchAP v
makeLatchAPWithoutInitialBlip = wrap (\p -> go (Just p) p)
  where
  go :: LatchAP v -> AudioParameter_ v -> CfLatchAP (MakeLatchAP v) (LatchAP v)
  go old new = wrap ((if old `cmpAP` Just new then Nothing else Just new) :< map unwrap (wrap (go (Just new))))

instance semigroupCfRate :: Semigroup v => Semigroup (CfLatchAP (MakeLatchAP v) (LatchAP v)) where
  append f0i f1i =
    let
      hd = ((extract f0i) <> (extract f1i))

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance monoidARate :: (Semigroup v, Eq v) => Monoid (ALatchAP v) where
  mempty = makeLatchAP

instance actualizeLatchAP :: Actualize (ALatchAP v) e (AudioParameter_ v) (CfLatchAP (MakeLatchAP v) (LatchAP v)) where
  actualize (MakeLatchAP r) _ b = r b

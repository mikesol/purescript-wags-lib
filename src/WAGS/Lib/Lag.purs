module WAGS.Lib.Lag where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import WAGS.Lib.Cofree (class Actualize, class ActualizeE, actualizeE)

newtype MakeLag a b
  = MakeLag (a -> b)

derive instance newtypeMakeLag :: Newtype (MakeLag a b) _

derive instance functorMakeLag :: Functor (MakeLag a)

derive newtype instance semigroupMakeLag :: (Semigroup a, Semigroup b) => Semigroup (MakeLag a b)

newtype CfLag f a
  = CfLag (Cofree f a)

derive instance newtypeCfLag :: Newtype (CfLag (MakeLag a) b) _

derive newtype instance functorCfLag :: Functor (CfLag (MakeLag a))

derive newtype instance extendCfLag :: Extend (CfLag (MakeLag a))

derive newtype instance comonadCfLag :: Comonad (CfLag (MakeLag a))

derive newtype instance comonadCofreeCfLag :: ComonadCofree (MakeLag a) (CfLag (MakeLag a))

type ALag (a :: Type)
  = MakeLag a (CfLag (MakeLag a) (Either a (Tuple a a)))

makeLag :: forall a. ALag a
makeLag = MakeLag (CfLag <<< ((:<) <$> Left <*> go))
  where
  go old = MakeLag ((:<) <$> (Right <<< Tuple old) <*> go)

instance semigroupCfLag :: Semigroup a => Semigroup (CfLag (MakeLag a) (Either a (Tuple a a))) where
  append f0i f1i =
    let
      hd = extract f0i <> extract f1i

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance monoidALag :: Semigroup a => Monoid (ALag a) where
  mempty = makeLag

instance actualizeLag :: Actualize (MakeLag a out) z a out where
  actualize = actualizeE

instance actualizeLagE :: ActualizeE (MakeLag a) z a where
  actualizeE (MakeLag r) _ a = r a

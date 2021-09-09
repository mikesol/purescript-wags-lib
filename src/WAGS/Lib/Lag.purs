module WAGS.Lib.Lag where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import WAGS.Lib.Cofree (composeComonadCofree, deferComposeComonadCofree)

type CfLag a
  = Cofree ((->) a) (Either a (Tuple a a))

type ALag (a :: Type)
  = a -> CfLag a

makeLag :: forall a. ALag a
makeLag = (:<) <$> Left <*> go
  where
  go old = (:<) <$> (Right <<< Tuple old) <*> go

withLag
  :: forall f a
   . Functor f
  => f (Cofree f a)
  -> f (Cofree f (Either a (Tuple a a)))
withLag = flip composeComonadCofree makeLag

withDeferredLag
  :: forall f a
   . Functor f
  => f (Cofree f a)
  -> f (Cofree f (Either a (Tuple a a)))
withDeferredLag = flip deferComposeComonadCofree makeLag

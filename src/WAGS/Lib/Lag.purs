module WAGS.Lib.Lag where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import WAGS.Lib.Cofree (convolveComonadCofreeChooseB, deferConvolveComonadCofreeChooseB)

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
withLag = flip (convolveComonadCofreeChooseB (\cont e b -> map (\cf -> cont cf (b (extract cf))) e)) makeLag

withDeferredLag
  :: forall f a
   . Functor f
  => f (Cofree f a)
  -> f (Cofree f (Either a (Tuple a a)))
withDeferredLag = flip (deferConvolveComonadCofreeChooseB (\cont e b -> map (\cf -> cont cf (b (extract cf))) e)) makeLag

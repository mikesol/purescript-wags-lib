module WAGS.Lib.Lag where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import WAGS.Lib.Cofree (convolveComonadCofreeChooseB)

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
  => (forall z. (Cofree f a -> z) -> f (Cofree f a) -> f z)
  -> f (Cofree f a)
  -> f (Cofree f (Either a (Tuple a a)))
withLag f i = convolveComonadCofreeChooseB (\cont e b -> f (\cf -> cont cf (b (extract cf))) e) i makeLag

module WAGS.Lib.Lag where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

type CfLag a
  = Cofree ((->) a) (Either a (Tuple a a))

type ALag (a :: Type)
  = a -> CfLag a

makeLag :: forall a. ALag a
makeLag = (:<) <$> Left <*> go
  where
  go old = (:<) <$> (Right <<< Tuple old) <*> go

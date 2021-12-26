module WAGS.Lib.Tidal.ObjectHack where

import Prelude

import Control.Monad.State (execState, get, put)
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign.Object as Object

catMaybes :: forall a. Object.Object (Maybe a) -> Object.Object a
catMaybes o = execState
  ( traverseWithIndex
      ( \k v -> do
          x <- get
          case v of
            Nothing -> pure unit
            Just v' -> put (Object.insert k v' x)
      )
      o
  )
  Object.empty
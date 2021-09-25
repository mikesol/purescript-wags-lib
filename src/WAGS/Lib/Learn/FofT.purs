module WAGS.Lib.Learn.FofT where

import Prelude

import Data.Identity (Identity(..))

class FofT f where
  toFofT :: f ~> (->) Number

instance toFOfT'Identity :: FofT Identity where
  toFofT (Identity a) = const a

instance toFOfT'FofT :: FofT ((->) Number) where
  toFofT = identity
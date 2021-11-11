module WAGS.Lib.Comonad where

import Prelude

import Control.Comonad (class Comonad, extract)

rewrap :: forall w f. Comonad w => Applicative f => w ~> f
rewrap = pure <<< extract
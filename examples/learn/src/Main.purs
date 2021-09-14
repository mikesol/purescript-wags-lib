module Main where

import Prelude

import Control.Comonad.Cofree (deferCofree)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Data.NonEmpty ((:|))
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)
import WAGS.Lib.Learn (component)
import WAGS.Lib.Stream (cycle)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy (component 60)
  , Tuple "array" $ proxy (component [ 60, 62, 64, 68, 73, 49, 41 ])
  , Tuple "cyclic array" $ proxy (component $ cycle $ 60 :| [ 62, 64, 68, 73, 49, 41 ])
  , Tuple "freq array" $ proxy (component $ cycle $ 440.0 :| [ 447.0, 483.0, 499.9999 ])
  , Tuple "cofree" $ proxy
      ( component
          ( let
              f x b =
                deferCofree \_ -> Tuple x
                  $ Identity
                  $ f (x + (if b then 2 else -2))
                      (if x >= 70 then false else if x <= 60 then true else b)
            in
              f 60 true
          )
      )
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Nothing
      }

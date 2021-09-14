module Test.Markov where

import Prelude

import App.Markov (getObservations)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Foldable (for_)
import Data.Typelevel.Num (d10, d2, d32, d8, toInt)
import Data.Vec ((+>))
import Data.Vec as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testMarkov :: Spec Unit
testMarkov = describe "Markov" do
  it "Returns correct dimensions for markov" do
    let options = { observations: d10
      , time: d32
      , states: d8
      , dimensions: d2
      , pi: (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
      , "A":
          ( (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
              +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
              +> (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
              +> (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
              +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
              +> (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
              +> (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
              +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
              +> V.empty
          )
      , mu:
          ( (-7.0 +> -8.0 +> V.empty)
              +> (-1.5 +> 3.7 +> V.empty)
              +> (-1.7 +> 1.2 +> V.empty)
              +> (-7.0 +> -8.0 +> V.empty)
              +> (-1.5 +> 3.7 +> V.empty)
              +> (-1.7 +> 1.2 +> V.empty)
              +> (-7.0 +> -8.0 +> V.empty)
              +> (-1.5 +> 3.7 +> V.empty)
              +> V.empty
          )
      , "Sigma":
          ( ( (0.12 +> -0.01 +> V.empty)
                +> (-0.01 +> 0.50 +> V.empty)
                +> V.empty
            )
              +>
                ( (0.21 +> 0.05 +> V.empty)
                    +> (0.05 +> 0.03 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.37 +> 0.35 +> V.empty)
                    +> (0.35 +> 0.44 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.12 +> -0.01 +> V.empty)
                    +> (-0.01 +> 0.50 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.21 +> 0.05 +> V.empty)
                    +> (0.05 +> 0.03 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.37 +> 0.35 +> V.empty)
                    +> (0.35 +> 0.44 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.12 +> -0.01 +> V.empty)
                    +> (-0.01 +> 0.50 +> V.empty)
                    +> V.empty
                )
              +>
                ( (0.21 +> 0.05 +> V.empty)
                    +> (0.05 +> 0.03 +> V.empty)
                    +> V.empty
                )
              +> V.empty
          )
      }
    res <- toAffE $ getObservations options
    let arr = V.toArray (map V.toArray res)
    A.length arr `shouldEqual` toInt options.observations
    for_ arr \i -> A.length i `shouldEqual` toInt options.time
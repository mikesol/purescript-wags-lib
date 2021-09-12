module App.Markov where

import Prelude

import Control.Promise (Promise)
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D3, toInt)
import Data.Vec as V
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

foreign import getObservations_
  :: { observations :: Int
     , time :: Int
     , states :: Int
     , dimensions :: Int
     , pi :: Array Number
     , "A" :: Array (Array Number)
     , mu :: Array (Array Number)
     , "Sigma" :: Array (Array (Array Number))
     }
  -> Effect (Promise (Array (Array Int)))

-- the return value has the additional constraint that
-- all of the integers will be between 0 and the number of
-- states
-- we use a vec for simplicity here: otherwise, we'd have to
-- constrain a heterogeneous structure
getObservations
  :: forall observations time states dimensions
   . Lt D3 observations
  => Lt D3 time
  => Nat observations
  => Nat time
  => Pos states
  => Pos dimensions
  => { observations :: observations
     , time :: time
     , states :: states
     , dimensions :: dimensions
     , pi :: V.Vec states Number
     , "A" :: V.Vec states (V.Vec states Number)
     , mu :: V.Vec states (V.Vec dimensions Number)
     , "Sigma" :: V.Vec states (V.Vec dimensions (V.Vec dimensions Number))
     }
  -> Effect (Promise (V.Vec observations (V.Vec time Int)))

getObservations { observations, time, states, dimensions, pi, "A": a, mu, "Sigma": sigma } = unsafeArr $ getObservations_
  { observations: toInt observations
  , time: toInt time
  , states: toInt states
  , dimensions: toInt dimensions
  , pi: V.toArray pi
  , "A": V.toArray (map V.toArray a)
  , mu: V.toArray (map V.toArray mu)
  , "Sigma": V.toArray (map V.toArray ((map <<< map) V.toArray sigma))
  }
  where
  unsafeArr :: Effect (Promise (Array (Array Int))) -> Effect (Promise (V.Vec observations (V.Vec time Int)))
  unsafeArr = unsafeCoerce
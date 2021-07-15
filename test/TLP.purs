module Test.TLP where

import Prelude
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Env (Env, env)
import Data.Identity (Identity(..))
import Data.Tuple.Nested ((/\))
import WAGS.Lib.Cofree (heads, tails)

envUA = env unit 1.0 :: Env Unit Number

myCF :: Cofree Identity Number
myCF = let x _ = deferCofree \_ -> 1.0 /\ (Identity (x unit)) in x unit

type ICF
  = Identity (Cofree Identity Number)

t0 :: { a :: Number, b :: { c :: Number, d :: Number, e :: { f :: Number } } }
t0 = heads { a: envUA, b: { c: envUA, d: envUA, e: { f: envUA } } }

t1 :: { a :: ICF, b :: { c :: ICF, d :: ICF, e :: { f :: ICF } } }
t1 = tails { a: myCF, b: { c: myCF, d: myCF, e: { f: myCF } } }

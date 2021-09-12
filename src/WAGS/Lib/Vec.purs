module WAGS.Lib.Vec where

import Data.Typelevel.Num (class Lt, class Nat, toInt')
import Data.Vec as V
import Type.Proxy (Proxy(..))

newtype VMHack (n :: Type) a b = VMHack (forall i. Nat i => Lt i n => i -> a -> b)

foreign import unsafeMapWithTypedIndex :: forall n a b. Int -> VMHack n a b -> V.Vec n a -> V.Vec n b

mapWithTypedIndex :: forall n a b. Nat n => (forall i. Nat i => Lt i n => i -> a -> b) -> V.Vec n a -> V.Vec n b
mapWithTypedIndex f a = unsafeMapWithTypedIndex (toInt' (Proxy :: _ n)) (VMHack f) a
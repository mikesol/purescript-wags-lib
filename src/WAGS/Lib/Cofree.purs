module WAGS.Lib.Cofree where

import Prelude hiding (Ordering(..))

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree as Cf
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat)
import Data.Vec as V
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Ordering (Ordering, LT, EQ, GT)
import Prim.Row (class Lacks, class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Run (SceneI)

--- heads and tails needs recursion down records to work
--- otherwise namespacing is impossible
data Tailz
  = Tailz

class Tailable a b | a -> b where
  tails :: a -> b

instance tailableRow :: HMap Tailz { | ii } { | oo } => Tailable { | ii } { | oo } where
  tails = hmap Tailz
else instance tailableVec :: (Nat n, Tailable i o) => Tailable (V.Vec n i) (V.Vec n o) where
  tails = map tails 
else instance tailableCf :: ComonadCofree f w => Tailable (w a) (f (w a)) where
  tails = unwrapCofree

instance tailzMapping ::
  Tailable i o =>
  Mapping Tailz i o where
  mapping Tailz = tails

data Headz
  = Headz

class Headable a b | a -> b where
  heads :: a -> b

instance headableRow :: HMap Headz { | ii } { | oo } => Headable { | ii } { | oo } where
  heads = hmap Headz
else instance headableVec :: (Nat n, Headable i o) => Headable (V.Vec n i) (V.Vec n o) where
  heads = map heads 
else instance headableCf :: Comonad w => Headable (w a) a where
  heads = extract

instance headzMapping ::
  Headable i o =>
  Mapping Headz i o where
  mapping Headz = heads

class Actualize n e r o | n e -> r o where
  actualize :: n -> e -> r -> o

instance actualizeIdentityComonad :: Actualize (Identity (Cf.Cofree Identity a)) e r (Cf.Cofree Identity a) where
  actualize (Identity c) _ _ = c

instance actualizeTuple :: (Actualize a e c x, Actualize b e d y) => Actualize (Tuple a b) e (Tuple c d) (Tuple x y) where
  actualize (Tuple a b) e (Tuple c d) = Tuple (actualize a e c) (actualize b e d)

instance actualizeVec :: (Nat n, Actualize a e c x) => Actualize (V.Vec n a) e (V.Vec n c) (V.Vec n x) where
  actualize n e r = V.zipWithE (\n' r' -> actualize n' e r') n r

instance actualizeEither :: (Actualize a e c x, Actualize b e d y) => Actualize (Either a b) e (Tuple c d) (Either x y) where
  actualize (Left a) e (Tuple c _) = Left (actualize a e c)
  actualize (Right b) e (Tuple _ d) = Right (actualize b e d)

instance actualizeRow :: Actualizes n e r o => Actualize { | n } e { | r } { | o } where
  actualize n e r = actualizes n e r

class ActualizeMany'' (cmp :: Ordering) (rln :: RowList Type) (rlr :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rln rlr n e -> r o where
  actualizeMany'' :: forall proxyCmp proxyRL. proxyCmp cmp -> proxyRL rln -> proxyRL rlr -> { | n } -> e -> { | r } -> { | o }

-- LT means that there is something in n that is not in r, so we give it unit
instance actualizeManyLT :: (ActualizeMany' c rowList n (SceneI trigger world analyserCallbacks) r x, IsSymbol a, Cons a b z' n, Actualize b (SceneI trigger world analyserCallbacks) Unit o, Lacks a x, Cons a o x yay) => ActualizeMany'' LT (RowList.Cons a b c) rowList n (SceneI trigger world analyserCallbacks) r yay where
  actualizeMany'' _ _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world analyserCallbacks) -> Unit -> o) (Record.get (Proxy :: _ a) n) e unit) (actualizeMany' (Proxy :: _ c) (Proxy :: _ rowList) n e r)

-- GT means that there is something in r that is not in n, so we ignore it
instance actualizeManyGT :: (ActualizeMany' rowList c n (SceneI trigger world analyserCallbacks) r yay) => ActualizeMany'' GT rowList (RowList.Cons a b c) n (SceneI trigger world analyserCallbacks) r yay where
  actualizeMany'' _ _ _ = actualizeMany' (Proxy :: _ rowList) (Proxy :: _ c)

-- EQ means they must be present in both
instance actualizeManyEQ :: (ActualizeMany' c v n (SceneI trigger world analyserCallbacks) r x, IsSymbol a, Cons a b z' n, Cons a m z'' r, Actualize b (SceneI trigger world analyserCallbacks) m o, Lacks a x, Cons a o x yay) => ActualizeMany'' EQ (RowList.Cons a b c) (RowList.Cons a m v) n (SceneI trigger world analyserCallbacks) r yay where
  actualizeMany'' _ _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world analyserCallbacks) -> m -> o) (Record.get (Proxy :: _ a) n) e (Record.get (Proxy :: _ a) r)) (actualizeMany' (Proxy :: _ c) (Proxy :: _ v) n e r)

class ActualizeMany' (rln :: RowList Type) (rlr :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rln rlr n e -> r o where
  actualizeMany' :: forall proxy. proxy rln -> proxy rlr -> { | n } -> e -> { | r } -> { | o }

instance actualizeMany'NilNil :: ActualizeMany' RowList.Nil RowList.Nil n (SceneI trigger world analyserCallbacks) r () where
  actualizeMany' _ _ _ _ _ = {}

instance actualizeMany'NilCons :: ActualizeMany' RowList.Nil (RowList.Cons a b c) n (SceneI trigger world analyserCallbacks) r () where
  actualizeMany' _ _ _ _ _ = {}

instance actualizeMany'ConsNil :: (ActualizeMany' c RowList.Nil n (SceneI trigger world analyserCallbacks) r x, IsSymbol a, Cons a b z' n, Actualize b (SceneI trigger world analyserCallbacks) Unit o, Lacks a x, Cons a o x yay) => ActualizeMany' (RowList.Cons a b c) RowList.Nil n (SceneI trigger world analyserCallbacks) r yay where
  actualizeMany' _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world analyserCallbacks) -> Unit -> o) (Record.get (Proxy :: _ a) n) e unit) (actualizeMany' (Proxy :: _ c) (Proxy :: _ RowList.Nil) n e r)

instance actualizeMany'ConsCons ::
  ( Sym.Compare a t cmp
  , ActualizeMany'' cmp (RowList.Cons a b c) (RowList.Cons t u v) n e r o
  ) =>
  ActualizeMany' (RowList.Cons a b c) (RowList.Cons t u v) n e r o where
  actualizeMany' = actualizeMany'' (Proxy :: _ cmp)

class Actualizes (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | n e -> r o where
  actualizes :: { | n } -> e -> { | r } -> { | o }

instance actualizesAll :: (RowToList n rln, RowToList r rlr, ActualizeMany' rln rlr n e r o) => Actualizes n e r o where
  actualizes = actualizeMany' (Proxy :: _ rln) (Proxy :: _ rlr)

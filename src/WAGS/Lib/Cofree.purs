module WAGS.Lib.Cofree where

import Prelude (Unit, unit)

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree as Cf
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Ordering (Ordering, LT, EQ, GT)
import Prim.Row (class Lacks, class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Run (SceneI)

data Tailz
  = Tailz

instance tailzMapping ::
  ComonadCofree f w =>
  Mapping Tailz (w a) (f (w a)) where
  mapping Tailz = unwrapCofree

tails :: forall ii oo. HMap Tailz { | ii } { | oo } => { | ii } -> { | oo }
tails = hmap Tailz

data Headz
  = Headz

instance headzMapping ::
  Comonad w =>
  Mapping Headz (w a) a where
  mapping Headz = extract

heads :: forall ii oo. HMap Headz { | ii } { | oo } => { | ii } -> { | oo }
heads = hmap Headz

class Actualize n e r o | n e -> r o where
  actualize :: n -> e -> r -> o

instance actualizeIdentityComonad :: Actualize (Identity (Cf.Cofree Identity a)) e r (Cf.Cofree Identity a) where
  actualize (Identity c) _ _ = c

class ActualizeMany'' (cmp :: Ordering) (rln :: RowList Type) (rlr :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rln rlr n e -> r o where
  actualizeMany'' :: forall proxyCmp proxyRL. proxyCmp cmp -> proxyRL rln -> proxyRL rlr -> { | n } -> e -> { | r } -> { | o }

-- LT means that there is something in n that is not in r, so we give it unit
instance actualizeManyLT :: (ActualizeMany' c rowList n (SceneI trigger world) r x, IsSymbol a, Cons a b z' n, Actualize b (SceneI trigger world) Unit o, Lacks a x, Cons a o x yay) => ActualizeMany'' LT (RowList.Cons a b c) rowList n (SceneI trigger world) r yay where
  actualizeMany'' _ _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world) -> Unit -> o) (Record.get (Proxy :: _ a) n) e unit) (actualizeMany' (Proxy :: _ c) (Proxy :: _ rowList) n e r)

-- GT means that there is something in r that is not in n, so we ignore it
instance actualizeManyGT :: (ActualizeMany' rowList c n (SceneI trigger world) r yay) => ActualizeMany'' GT rowList (RowList.Cons a b c) n (SceneI trigger world) r yay where
  actualizeMany'' _ _ _ = actualizeMany' (Proxy :: _ rowList) (Proxy :: _ c)

-- EQ means they must be present in both
instance actualizeManyEQ :: (ActualizeMany' c v n (SceneI trigger world) r x, IsSymbol a, Cons a b z' n, Cons a m z'' r, Actualize b (SceneI trigger world) m o, Lacks a x, Cons a o x yay) => ActualizeMany'' EQ (RowList.Cons a b c) (RowList.Cons a m v) n (SceneI trigger world) r yay where
  actualizeMany'' _ _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world) -> m -> o) (Record.get (Proxy :: _ a) n) e (Record.get (Proxy :: _ a) r)) (actualizeMany' (Proxy :: _ c) (Proxy :: _ v) n e r)

class ActualizeMany' (rln :: RowList Type) (rlr :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rln rlr n e -> r o where
  actualizeMany' :: forall proxy. proxy rln -> proxy rlr -> { | n } -> e -> { | r } -> { | o }

instance actualizeMany'NilNil :: ActualizeMany' RowList.Nil RowList.Nil n (SceneI trigger world) r () where
  actualizeMany' _ _ _ _ _ = {}

instance actualizeMany'NilCons :: ActualizeMany' RowList.Nil (RowList.Cons a b c) n (SceneI trigger world) r () where
  actualizeMany' _ _ _ _ _ = {}

instance actualizeMany'ConsNil :: (ActualizeMany' c RowList.Nil n (SceneI trigger world) r x, IsSymbol a, Cons a b z' n, Actualize b (SceneI trigger world) Unit o, Lacks a x, Cons a o x yay) => ActualizeMany' (RowList.Cons a b c) RowList.Nil n (SceneI trigger world) r yay where
  actualizeMany' _ _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world) -> Unit -> o) (Record.get (Proxy :: _ a) n) e unit) (actualizeMany' (Proxy :: _ c) (Proxy :: _ RowList.Nil) n e r)

instance actualizeMany'ConsCons :: (Sym.Compare a t cmp
  , ActualizeMany'' cmp (RowList.Cons a b c) (RowList.Cons t u v) n e r o) => ActualizeMany' (RowList.Cons a b c) (RowList.Cons t u v) n e r o where
  actualizeMany' = actualizeMany'' (Proxy :: _ cmp)

class Actualizes (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | n e -> r o where
  actualizes :: { | n } -> e -> { | r } -> { | o }

instance actualizesAll :: (RowToList n rln, RowToList r rlr, ActualizeMany' rln rlr n e r o) => Actualizes n e r o where
  actualizes = actualizeMany' (Proxy :: _ rln) (Proxy :: _ rlr)
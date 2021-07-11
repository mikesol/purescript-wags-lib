module WAGS.Lib.Cofree where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree as Cf
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row (class Lacks, class Union, class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
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

class Obliterate i where
  obliterate :: i -> Unit

instance obliterateAll :: Obliterate i where
  obliterate _ = unit

class AddUnit i l r | i l -> r where
  addUnit :: { | i } -> { | l } -> { | r }

class ObliterateR (i :: RowList Type) (o :: Row Type) | i -> o where
  obliterateR :: Proxy i -> { | o }

instance obliterateR'Nil :: ObliterateR RowList.Nil () where
  obliterateR _ = {}

instance obliterateR'Cons :: (ObliterateR c x, IsSymbol a, Obliterate b, Lacks a x, Cons a Unit x yay) => ObliterateR (RowList.Cons a b c) yay where
  obliterateR _ = Record.insert (Proxy :: _ a) unit (obliterateR (Proxy :: _ c))

instance addUnitAll :: (RowToList ii iil, ObliterateR iil oo, Union l oo r) => AddUnit ii l r where
  addUnit i l = Record.union l (obliterateR (Proxy :: _ iil))

class ActualizeMany' (rl :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rl n e -> r o where
  actualizeMany' :: Proxy rl -> { | n } -> e -> { | r } -> { | o }

instance actualizeMany'Nil :: ActualizeMany' RowList.Nil n (SceneI trigger world) r () where
  actualizeMany' _ _ _ _ = {}

instance actualizeMany'Cons :: (ActualizeMany' c n (SceneI trigger world) r x, IsSymbol a, Cons a b z' n, Cons a m z'' r, Actualize b (SceneI trigger world) m o, Lacks a x, Cons a o x yay) => ActualizeMany' (RowList.Cons a b c) n (SceneI trigger world) r yay where
  actualizeMany' _ n e r = Record.insert (Proxy :: _ a) ((actualize :: b -> (SceneI trigger world) -> m -> o) (Record.get (Proxy :: _ a) n) e (Record.get (Proxy :: _ a) r)) (actualizeMany' (Proxy :: _ c) n e r)

actualizes ::
  forall toActualize rl e extraInfo extraInfoWithUnit out.
  RowToList toActualize rl =>
  AddUnit toActualize extraInfo extraInfoWithUnit =>
  ActualizeMany' rl toActualize e extraInfoWithUnit out =>
  { | toActualize } -> e -> { | extraInfo } -> { | out }
actualizes toActualize e extraInfo = actualizeMany' (Proxy :: _ rl) toActualize e (addUnit toActualize extraInfo)

module WAGS.Lib.Cofree where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Control.Comonad.Cofree as Cf
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row (class Lacks, class Union, class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Run (SceneI)

data Tailz = Tailz

instance tailzMapping ::
  (Tailable n o) =>
  Mapping Tailz n o where
  mapping Tailz = tail

tails :: forall ii oo. HMap Tailz { | ii } { | oo } => { | ii } -> { | oo }
tails = (hmap :: Tailz -> { | ii } -> { | oo }) Tailz

newtype Simply a
  = Simply a

derive instance newtypeSimply :: Newtype (Simply a) _

derive instance functorSimply :: Functor Simply

instance extendSimply :: Extend Simply where
  extend ej i = Simply (ej i)

instance comonadSimply :: Comonad Simply where
  extract (Simply i) = i

class Tailable i o where
  tail :: i -> o

instance tailableSimply :: Tailable (Simply a) (Simply a) where
  tail = identity

instance tailableCofree :: Tailable (Cf.Cofree f a) (f (Cf.Cofree f a)) where
  tail = Cf.tail
else instance tailableRate :: Newtype n (f (Cf.Cofree f a)) => Tailable (Cf.Cofree f a) n where
  tail = wrap <<< Cf.tail

class Actualize n e r o | n e r -> o where
  actualize :: n -> e -> r -> o

instance actualizeIdentityComonad :: Actualize (Identity (Cf.Cofree Identity Boolean)) e r (Cf.Cofree Identity Boolean) where
  actualize (Identity c) _ _ = c

instance actualizeSimply :: Actualize (Simply r) e r (Simply r) where
  actualize a _ _ = a

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

class ActualizeMany' (rl :: RowList Type) (n :: Row Type) (e :: Type) (r :: Row Type) (o :: Row Type) | rl n e r -> o where
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

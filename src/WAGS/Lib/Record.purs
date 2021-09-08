module WAGS.Lib.Record
  ( class UnwrappableRow
  , class AppliableRow
  , unwrapRecord
  , applyRecord
  ) where

import Data.Newtype (class Newtype)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

class UnwrappableRow (rl :: RL.RowList Type) (r :: Row Type) | rl -> r

instance unwrappableRow :: UnwrappableRow RL.Nil ()

instance unwrappableConsRec ::
  ( UnwrappableRow rest r'
  , RL.RowToList rc rc'
  , UnwrappableRow rc' o
  , Cons sym { | o } r' r
  ) =>
  UnwrappableRow (RL.Cons sym { | rc } rest) r
else instance unwrappableCons ::
  ( UnwrappableRow rest r'
  , Newtype nt unwrapped
  , Cons sym unwrapped r' r
  ) =>
  UnwrappableRow (RL.Cons sym nt rest) r

unwrapRecord :: forall rl ri ro. RL.RowToList ri rl => UnwrappableRow rl ro => { | ri } -> { | ro }
unwrapRecord = unsafeCoerce

class AppliableRow (rl :: RL.RowList Type) (r1 :: Row Type) (r2 :: Row Type) | rl -> r1 r2

instance appliableRow :: AppliableRow RL.Nil () ()

instance appliableConsRec ::
  ( AppliableRow rest r1' r2'
  , RL.RowToList rc rc'
  , AppliableRow rc' o1 o2
  , Cons sym { | o1 } r1' r1
  , Cons sym { | o2 } r2' r2
  ) =>
  AppliableRow (RL.Cons sym { | rc } rest) r1 r2
else instance appliableCons ::
  ( AppliableRow rest r1' r2'
  , Cons sym a r1' r2
  , Cons sym b r2' r2
  ) =>
  AppliableRow (RL.Cons sym (a -> b) rest) r1 r2

foreign import applyRecordImpl :: forall r1 r2 r3. { | r1 } -> { | r2 } -> { | r3 }

applyRecord :: forall rl rab ra rb. RL.RowToList rab rl => AppliableRow rl ra rb => { | rab } -> { | ra } -> { | rb }
applyRecord = applyRecordImpl
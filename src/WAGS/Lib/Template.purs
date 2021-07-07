module WAGS.Lib.Template where

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, type (:*), D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, toInt')
import Data.Vec as V
import Prim.Row (class Lacks, class Cons)
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Peano as P
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (gain)
import WAGS.Graph.AudioUnit (Gain)
import WAGS.Graph.Parameter (AudioParameter)


class Nat n <= NatToPeano (n :: Type) (p :: P.Nat) | n -> p

instance natToPeanoD0 :: NatToPeano D0 P.Z

instance natToPeanoD1 :: NatToPeano D1 (P.Succ P.Z)

instance natToPeanoD2 :: NatToPeano D2 (P.Succ (P.Succ P.Z))

instance natToPeanoD3 :: NatToPeano D3 (P.Succ (P.Succ (P.Succ P.Z)))

instance natToPeanoD4 :: NatToPeano D4 (P.Succ (P.Succ (P.Succ (P.Succ P.Z))))

instance natToPeanoD5 :: NatToPeano D5 (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ P.Z)))))

instance natToPeanoD6 :: NatToPeano D6 (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ P.Z))))))

instance natToPeanoD7 :: NatToPeano D7 (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ P.Z)))))))

instance natToPeanoD8 :: NatToPeano D8 (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ P.Z))))))))

instance natToPeanoD9 :: NatToPeano D9 (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ (P.Succ P.Z)))))))))

instance natToPeanoDCons :: (Nat (x :* y), NatToPeano x sx, NatToPeano y sy, P.SumNat sx sy o) => NatToPeano (x :* y) o

class Nat n <= NatToSym (n :: Type) (s :: Symbol) | n -> s

instance natToSymD0 :: NatToSym D0 "D0"

instance natToSymD1 :: NatToSym D1 "D1"

instance natToSymD2 :: NatToSym D2 "D2"

instance natToSymD3 :: NatToSym D3 "D3"

instance natToSymD4 :: NatToSym D4 "D4"

instance natToSymD5 :: NatToSym D5 "D5"

instance natToSymD6 :: NatToSym D6 "D6"

instance natToSymD7 :: NatToSym D7 "D7"

instance natToSymD8 :: NatToSym D8 "D8"

instance natToSymD9 :: NatToSym D9 "D9"

instance natToSymDCons :: (Nat (x :* y), NatToSym x sx, NatToSym y sy, Symbol.Append sx "_" s0, Symbol.Append s0 sy o) => NatToSym (x :* y) o

class SuffixAllRecords (s :: Symbol) (i :: Row Type) (o :: Row Type) | s i -> o where
  suffixize :: forall proxy. proxy s -> { | i } -> { | o }

instance suffixAllRecordsAll :: (RowList.RowToList i rl, SuffixAllRecordsRL rl s i o) => SuffixAllRecords s i o where
  suffixize = suffixizeRL (Proxy :: _ rl)

class SuffixAllRecordsRL (rl :: RowList Type) (s :: Symbol) (i :: Row Type) (o :: Row Type) | rl s i -> o where
  suffixizeRL :: forall proxyA proxyB. proxyA rl -> proxyB s -> { | i } -> { | o }

instance suffixAllRecordsRLCons ::
  ( SuffixAllRecordsRL c s i o'
  , IsSymbol a
  , IsSymbol aSuffix
  , Cons a b x i
  , Symbol.Append a s aSuffix
  , Lacks aSuffix o'
  , Cons aSuffix b o' o
  ) =>
  SuffixAllRecordsRL (RowList.Cons a b c) s i o where
  suffixizeRL _ _ i = Record.insert (Proxy :: _ aSuffix) (Record.get (Proxy :: _ a) i) (suffixizeRL (Proxy :: _ c) (Proxy :: _ s) i)

instance suffixAllRecordsRLNil :: SuffixAllRecordsRL RowList.Nil s i () where
  suffixizeRL _ _ _ = {}

class SuffixAllRecordsRec (s :: Symbol) i o | s i -> o where
  suffixizeRec :: forall proxy. proxy s -> i -> o

instance suffixAllRecordsRec :: SuffixAllRecords s i o => SuffixAllRecordsRec s { | i } { | o } where
  suffixizeRec = suffixize

instance suffixAllRecordsTupRec :: SuffixAllRecords s i o => SuffixAllRecordsRec s (a /\ { | i }) (a /\ { | o }) where
  suffixizeRec s (a /\ b) = a /\ suffixize s b

class PoolWithTemplate' (suffix :: Symbol) (n :: Type) (a :: Type) (g :: Type) (o :: Row Type) | suffix n a g -> o where
  fromTemplate' :: forall proxy. proxy suffix -> V.Vec n a -> (Int -> a -> g) -> { | o }

instance poolWithTemplate'D0 :: PoolWithTemplate' suffix D0 a g () where
  fromTemplate' _ _ _ = {}
else instance poolWithTemplate'D ::
  ( Pred n n'
  , PoolWithTemplate' suffix n' a g x
  , NatToSym n sn
  , Symbol.Append "busFor_" sn s0
  , Symbol.Append s0 suffix sym
  , Symbol.Append "_unitFor_" sn s1
  , Symbol.Append s1 suffix sym'
  , SuffixAllRecordsRec sym' g gg
  , Cons sym gg x o
  , Lacks sym x
  , IsSymbol sym
  ) =>
  PoolWithTemplate' suffix n a g o where
  fromTemplate' px v fa =
    let
      uc = V.uncons v
    in
      Record.insert (Proxy :: _ sym)
        (suffixizeRec (Proxy :: _ sym') (fa (toInt' (Proxy :: _ n)) uc.head))
        (fromTemplate' px uc.tail fa)

class
  Pos n <= PoolWithTemplate (suffix :: Symbol) (n :: Type) (a :: Type) (g :: Type) (o :: Type) | suffix n a g -> o where
  fromTemplate :: forall proxy. proxy suffix -> V.Vec n a -> (Int -> a -> g) -> o

instance poolWithTemplateAll :: (Pos n, PoolWithTemplate' suffix n a g o) => PoolWithTemplate suffix n a g (Gain AudioParameter /\ { | o }) where
  fromTemplate a b c = gain 1.0 (fromTemplate' a b c)
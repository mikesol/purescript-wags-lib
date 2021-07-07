module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (_1, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
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
import WAGS.Graph.AudioUnit (APOnOff, Gain, OnOff(..))
import WAGS.Graph.Parameter (AudioParameter, AudioParameter_(..), ff)
import WAGS.Lib.Emitter (AnEmitter(..), fEmitter, makeEmitter)
import WAGS.Lib.Impulse (ABlip(..), makeBlip)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)

-- use array as it is faster
type TimeHeadroomOffsets (rest :: Type)
  = { time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } }

type TimeHeadroomRate
  = { time :: Number, headroom :: Number, rate :: Number }

type BufferInternal (rest :: Type)
  = { startedAt :: Number, env :: SAPFofT, rest :: rest }

type Buffy (rest :: Type)
  = { gain :: AudioParameter, onOff :: APOnOff, rest :: rest }

unchangingPiecewise :: NonEmpty List (Number /\ Number)
unchangingPiecewise = (0.0 /\ 1.0) :| Nil

type InternalState (rest :: Type)
  = { bufferInternal :: Maybe (BufferInternal rest), buffy :: Maybe (Buffy rest) }

notPlaying :: forall (rest :: Type). InternalState rest
notPlaying = { bufferInternal: Nothing, buffy: Nothing }

type BuffyVec (n :: Type) (rest :: Type)
  = V.Vec n (Maybe (Buffy rest))

type BuffyStream (n :: Type) (rest :: Type)
  = TimeHeadroomOffsets rest ->
    Cofree ((->) (TimeHeadroomOffsets rest)) (BuffyVec n rest)

type HotBuffyStream (n :: Type)
  = TimeHeadroomRate ->
    Cofree ((->) TimeHeadroomRate) (BuffyVec n Unit)

newtype ABufferPool n r
  = ABufferPool (BuffyStream n r)

derive instance newtypeABufferPool :: Newtype (ABufferPool n r) _

newtype AHotBufferPool n
  = AHotBufferPool (HotBuffyStream n)

derive instance newtypeAHotBufferPool :: Newtype (AHotBufferPool n) _

newtype ASnappyBufferPool n
  = ASnappyBufferPool (HotBuffyStream n)

derive instance newtypeASnappyBufferPool :: Newtype (ASnappyBufferPool n) _

makeHotBufferPool ::
  forall n.
  Pos n =>
  { prevTime :: Number, startsAt :: Number } ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  AHotBufferPool n
makeHotBufferPool ptsa dur pwf = AHotBufferPool (go emitter buffer)
  where
  (AnEmitter emitter) = makeEmitter ptsa

  (ABufferPool buffer) = makeBufferPool' (Proxy :: _ n) dur pwf

  go e b { time, headroom, rate } = head bnow :< go (tail enow) (tail bnow)
    where
    enow = e { time, headroom, rate }

    bnow = b { time, headroom, offsets: map { rest: unit, offset: _ } (head enow) }

makeSnappyBufferPool ::
  forall n.
  Pos n =>
  { prevTime :: Number, startsAt :: Number } ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  AHotBufferPool n
makeSnappyBufferPool ptsa dur pwf = AHotBufferPool (go blip buffer)
  where
  (ABlip blip) = makeBlip

  (ABufferPool buffer) = makeBufferPool dur pwf

  go e b { time, headroom, rate } = head bnow :< go (tail enow) (tail bnow)
    where
    emitted = fEmitter rate { time, headroom }

    enow = e (isJust emitted)

    bnow =
      b
        { time
        , headroom
        , offsets:
            if (head enow) then
              fromMaybe [] (pure <<< { offset: _, rest: unit } <$> emitted)
            else
              []
        }

makeBufferPool ::
  forall n r.
  Pos n =>
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  ABufferPool n r
makeBufferPool = makeBufferPool' (Proxy :: _ n)

makeBufferPool' ::
  forall n r.
  Pos n =>
  Proxy n ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  ABufferPool n r
makeBufferPool' _ dur pwf = ABufferPool (go 0 (V.fill (const (Nothing :: Maybe (BufferInternal r)))))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  realPwf :: NonEmpty List (Number /\ Number)
  realPwf = fromMaybe unchangingPiecewise pwf

  refresh :: { time :: Number, headroom :: Number, offset :: Number, rest :: r } -> OnOff -> InternalState r
  refresh { time, headroom, offset, rest } onOff =
    let
      sapfot = makePiecewise (map (over _1 (add (time + offset))) realPwf)

      now = sapfot { time, headroom }
    in
      { bufferInternal: Just { startedAt: time + offset, env: tail now, rest }, buffy: Just { gain: head now, onOff: ff offset (pure onOff), rest } }

  -- cPos myPos
  maybeBufferToGainOnOff ::
    Int ->
    TimeHeadroomOffsets r ->
    Int ->
    Maybe (BufferInternal r) ->
    InternalState r
  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos Nothing
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset, rest } On
    | otherwise = notPlaying

  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos (Just { startedAt, env, rest: prevRest })
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset, rest } OffOn
    | Just dur' <- dur
    , time - startedAt > dur' = notPlaying
    | otherwise =
      let
        now = env { time, headroom }
      in
        { bufferInternal: Just { startedAt, env: tail now, rest: prevRest }, buffy: Just { gain: head now, onOff: pure On, rest: prevRest } }

  go ::
    Int ->
    V.Vec n (Maybe (BufferInternal r)) ->
    TimeHeadroomOffsets r ->
    Cofree ((->) (TimeHeadroomOffsets r)) (V.Vec n (Maybe (Buffy r)))
  go cIdx v tht@{ offsets } = map _.buffy internalStates :< go ((cIdx + A.length offsets) `mod` len) (map _.bufferInternal internalStates)
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

instance semigroupBufferPool :: (Pos n, Semigroup r) => Semigroup (ABufferPool n r) where
  append (ABufferPool i0) (ABufferPool i1) = ABufferPool f
    where
    f :: TimeHeadroomOffsets r -> Cofree ((->) (TimeHeadroomOffsets r)) (BuffyVec n r)
    f tho =
      ( V.zipWithE
          ( \a b -> case a, b of
              Nothing, Nothing -> Nothing
              Nothing, Just x -> Just x
              Just x, Nothing -> Just x
              Just x, Just y ->
                Just
                  { gain: x.gain + y.gain / (pure 2.0)
                  , onOff:
                      case x.onOff, y.onOff of
                        i@(AudioParameter { param: (Just On) }), j -> i
                        i, j@(AudioParameter { param: (Just On) }) -> j
                        i@(AudioParameter { param: (Just OffOn) }), j -> i
                        i, j@(AudioParameter { param: (Just OffOn) }) -> j
                        i, j -> i
                  , rest: x.rest <> y.rest
                  }
          )
          (head i0r)
          (head i1r)
      )
        :< (unwrap (append ((wrap (tail i0r)) :: ABufferPool n r) (wrap (tail i1r))))
      where
      i0r = i0 tho

      i1r = i1 tho

instance monoidBufferPool :: (Semigroup r, Pos n) => Monoid (ABufferPool n r) where
  mempty = makeBufferPool Nothing Nothing

bGain :: forall r. Maybe (Buffy r) -> AudioParameter
bGain = maybe (pure 0.0) _.gain

bOnOff :: forall r. Maybe (Buffy r) -> APOnOff
bOnOff = maybe (pure Off) _.onOff

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

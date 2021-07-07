module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (AudioParameter, AudioParameter_(..), ff)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Lib.Emitter (AnEmitter(..), fEmitter, makeEmitter)
import WAGS.Lib.Impulse (ABlip(..), makeBlip)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)
import WAGS.Run (SceneI(..))

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
  ASnappyBufferPool n
makeSnappyBufferPool ptsa dur pwf = ASnappyBufferPool (go blip buffer)
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

-- todo: code dup, merge with below
appendUBP :: forall nt n. Pos n => Semigroup nt => Newtype nt (TimeHeadroomRate -> Cofree ((->) TimeHeadroomRate) (BuffyVec n Unit)) => nt -> nt -> nt
appendUBP i0' i1' = wrap f
  where
  i0 = unwrap i0'
  i1 = unwrap i1'
  f :: TimeHeadroomRate -> Cofree ((->) TimeHeadroomRate) (BuffyVec n Unit)
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
      :< (unwrap (append ((wrap (tail i0r)) :: nt) (wrap (tail i1r))))
    where
    i0r = i0 tho

    i1r = i1 tho

instance semigroupHotBufferPool :: Pos n => Semigroup (AHotBufferPool n) where
  append = appendUBP

instance semigroupSnappyBufferPool :: Pos n => Semigroup (ASnappyBufferPool n) where
  append = appendUBP

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

instance monoidHotBufferPool :: (Pos n) => Monoid (AHotBufferPool n) where
  mempty = makeHotBufferPool { startsAt: 0.0, prevTime: 0.0 } Nothing Nothing

instance monoidSnappyBufferPool :: (Pos n) => Monoid (ASnappyBufferPool n) where
  mempty = makeSnappyBufferPool { startsAt: 0.0, prevTime: 0.0 } Nothing Nothing

bGain :: forall r. Maybe (Buffy r) -> AudioParameter
bGain = maybe (pure 0.0) _.gain

bOnOff :: forall r. Maybe (Buffy r) -> APOnOff
bOnOff = maybe (pure Off) _.onOff

instance actualizeHotBufferPool :: Actualize (AHotBufferPool n) (SceneI a b) Number (Cofree ((->) TimeHeadroomRate) (BuffyVec n Unit)) where
  actualize (AHotBufferPool r) (SceneI { time, headroom }) rate = r { time, headroom: toNumber headroom / 1000.0, rate }

instance actualizeSnappyBufferPool :: Actualize (ASnappyBufferPool n) (SceneI a b) Number (Cofree ((->) TimeHeadroomRate) (BuffyVec n Unit)) where
  actualize (ASnappyBufferPool r) (SceneI { time, headroom }) rate = r { time, headroom: toNumber headroom / 1000.0, rate }

module WAGS.Lib.BufferPool where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
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
import WAGS.Lib.Blip (ABlip, MakeBlip(..), makeBlip)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Lib.Emitter (MakeEmitter(..), AnEmitter, fEmitter, makeEmitter)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)
import WAGS.Run (SceneI(..))

type TimeHeadroomOffsets rest
  = { time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } }

type TimeHeadroomFreq
  = { time :: Number, headroom :: Number, freq :: Number }

newtype MakeBufferPoolWithRest rest a
  = MakeBufferPoolWithRest ({ time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } } -> a)

derive instance newtypeMakeBufferPoolWithRest :: Newtype (MakeBufferPoolWithRest rest a) _

derive instance functorMakeBufferPoolWithRest :: Functor (MakeBufferPoolWithRest rest)

derive newtype instance semigroupMakeBufferPoolWithRest :: Semigroup a => Semigroup (MakeBufferPoolWithRest rest a)

newtype MakeHotBufferPool a
  = MakeHotBufferPool (TimeHeadroomFreq -> a)

derive instance newtypeMakeHotBufferPool :: Newtype (MakeHotBufferPool a) _

derive instance functorMakeHotBufferPool :: Functor MakeHotBufferPool

derive newtype instance semigroupMakeHotBufferPool :: Semigroup a => Semigroup (MakeHotBufferPool a)

newtype MakeSnappyBufferPool a
  = MakeSnappyBufferPool (TimeHeadroomFreq -> a)

derive instance newtypeMakeSnappyBufferPool :: Newtype (MakeSnappyBufferPool a) _

derive instance functorMakeSnappyBufferPool :: Functor MakeSnappyBufferPool

derive newtype instance semigroupMakeSnappyBufferPool :: Semigroup a => Semigroup (MakeSnappyBufferPool a)

type BufferInternal rest
  = { startedAt :: Number, env :: SAPFofT, rest :: rest }

newtype Buffy rest
  = Buffy { gain :: AudioParameter, onOff :: APOnOff, rest :: rest }

derive instance newtypeBuffy :: Newtype (Buffy rest) _

unchangingPiecewise :: NonEmpty List (Number /\ Number)
unchangingPiecewise = (0.0 /\ 1.0) :| Nil

type InternalState rest
  = { bufferInternal :: Maybe (BufferInternal rest), buffy :: Maybe (Buffy rest) }

notPlaying :: forall (rest :: Type). InternalState rest
notPlaying = { bufferInternal: Nothing, buffy: Nothing }

type BuffyVec (n :: Type) (rest :: Type)
  = V.Vec n (Maybe (Buffy rest))

newtype CfBufferPool f a
  = CfBufferPool (Cofree f a)

derive instance newtypeCfBufferPool :: Newtype (CfBufferPool (MakeBufferPoolWithRest r) (BuffyVec n r)) _

derive newtype instance functorCfBufferPool :: Functor (CfBufferPool (MakeBufferPoolWithRest r))

derive newtype instance extendCfBufferPool :: Extend (CfBufferPool (MakeBufferPoolWithRest r))

derive newtype instance comonadCfBufferPool :: Comonad (CfBufferPool (MakeBufferPoolWithRest r))

derive newtype instance comonadCofreeCfBufferPool :: ComonadCofree (MakeBufferPoolWithRest r) (CfBufferPool (MakeBufferPoolWithRest r))

type ABufferPool n rest
  = MakeBufferPoolWithRest rest (CfBufferPool (MakeBufferPoolWithRest rest) (BuffyVec n rest))

newtype CfHotBufferPool f a
  = CfHotBufferPool (Cofree f a)

derive instance newtypeCfHotBufferPool :: Newtype (CfHotBufferPool MakeHotBufferPool (BuffyVec n Unit)) _

derive newtype instance functorCfHotBufferPool :: Functor (CfHotBufferPool MakeHotBufferPool)

derive newtype instance extendCfHotBufferPool :: Extend (CfHotBufferPool MakeHotBufferPool)

derive newtype instance comonadCfHotBufferPool :: Comonad (CfHotBufferPool MakeHotBufferPool)

derive newtype instance comonadCofreeCfHotBufferPool :: ComonadCofree MakeHotBufferPool (CfHotBufferPool MakeHotBufferPool)

type AHotBufferPool n
  = MakeHotBufferPool (CfHotBufferPool MakeHotBufferPool (BuffyVec n Unit))

newtype CfSnappyBufferPool f a
  = CfSnappyBufferPool (Cofree f a)

derive instance newtypeCfSnappyBufferPool :: Newtype (CfSnappyBufferPool MakeSnappyBufferPool (BuffyVec n Unit)) _

derive newtype instance functorCfSnappyBufferPool :: Functor (CfSnappyBufferPool MakeSnappyBufferPool)

derive newtype instance extendCfSnappyBufferPool :: Extend (CfSnappyBufferPool MakeSnappyBufferPool)

derive newtype instance comonadCfSnappyBufferPool :: Comonad (CfSnappyBufferPool MakeSnappyBufferPool)

derive newtype instance comonadCofreeCfSnappyBufferPool :: ComonadCofree MakeSnappyBufferPool (CfSnappyBufferPool MakeSnappyBufferPool)

type ASnappyBufferPool n
  = MakeSnappyBufferPool (CfSnappyBufferPool MakeSnappyBufferPool (BuffyVec n Unit))

makeBufferPool' ::
  forall n r.
  Pos n =>
  Proxy n ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  ABufferPool n r
makeBufferPool' _ dur pwf = MakeBufferPoolWithRest (go 0 (V.fill (const (Nothing :: Maybe (BufferInternal r)))))
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
      { bufferInternal: Just { startedAt: time + offset, env: unwrapCofree now, rest }, buffy: Just $ wrap { gain: extract now, onOff: ff offset (pure onOff), rest } }

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
        { bufferInternal: Just { startedAt, env: unwrapCofree now, rest: prevRest }, buffy: Just $ wrap { gain: extract now, onOff: pure On, rest: prevRest } }

  go ::
    Int ->
    V.Vec n (Maybe (BufferInternal r)) ->
    TimeHeadroomOffsets r ->
    CfBufferPool (MakeBufferPoolWithRest r) (BuffyVec n r)
  go cIdx v tht@{ offsets } =
    CfBufferPool
      ( map _.buffy internalStates
          :< map unwrap
              ( wrap
                  ( go ((cIdx + A.length offsets) `mod` len)
                      (map _.bufferInternal internalStates)
                  )
              )
      )
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

makeBufferPool ::
  forall n r.
  Pos n =>
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  ABufferPool n r
makeBufferPool = makeBufferPool' (Proxy :: _ n)

makeHotBufferPool ::
  forall n.
  Pos n =>
  { prevTime :: Number, startsAt :: Number } ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  AHotBufferPool n
makeHotBufferPool ptsa dur pwf = wrap (go emitter buffer)
  where
  emitter = makeEmitter ptsa

  buffer = makeBufferPool' (Proxy :: _ n) dur pwf

  go :: AnEmitter -> ABufferPool n Unit -> TimeHeadroomFreq -> CfHotBufferPool MakeHotBufferPool (BuffyVec n Unit)
  go (MakeEmitter e) (MakeBufferPoolWithRest b) { time, headroom, freq } =
    wrap
      ( extract bnow
          :< map unwrap (wrap (go (unwrapCofree enow) (unwrapCofree bnow)))
      )
    where
    enow = e { time, headroom, freq }

    bnow = b { time, headroom, offsets: map { rest: unit, offset: _ } (extract enow) }

makeSnappyBufferPool ::
  forall n.
  Pos n =>
  { prevTime :: Number, startsAt :: Number } ->
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  ASnappyBufferPool n
makeSnappyBufferPool ptsa dur pwf = wrap (go blip buffer)
  where
  blip = makeBlip

  buffer = makeBufferPool dur pwf

  go :: ABlip -> ABufferPool n Unit -> TimeHeadroomFreq -> CfSnappyBufferPool MakeSnappyBufferPool (BuffyVec n Unit)
  go (MakeBlip e) (MakeBufferPoolWithRest b) { time, headroom, freq } = wrap (extract bnow :< map unwrap (wrap (go (unwrapCofree enow) (unwrapCofree bnow))))
    where
    emitted = fEmitter freq { time, headroom }

    enow = e (isJust emitted)

    bnow =
      b
        { time
        , headroom
        , offsets:
            if (extract enow) then
              fromMaybe [] (pure <<< { offset: _, rest: unit } <$> emitted)
            else
              []
        }

apBV ::
  forall w f a n.
  Newtype (w f (BuffyVec n a)) (Cofree f (BuffyVec n a)) =>
  Semigroup (f (w f (BuffyVec n a))) =>
  Functor f =>
  Semigroup a =>
  ComonadCofree f (w f) =>
  Comonad (w f) =>
  w f (BuffyVec n a) -> w f (BuffyVec n a) -> w f (BuffyVec n a)
apBV i0r i1r =
  wrap
    ( ( V.zipWithE
          ( \a b -> case a, b of
              Nothing, Nothing -> Nothing
              Nothing, Just x -> Just x
              Just x, Nothing -> Just x
              Just (Buffy x), Just (Buffy y) ->
                Just
                  $ wrap
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
          (extract i0r)
          (extract i1r)
      )
        :< map unwrap (unwrapCofree i0r <> unwrapCofree i1r)
    )

instance semigroupBufferPool :: (Pos n, Semigroup rest) => Semigroup (CfBufferPool (MakeBufferPoolWithRest rest) (BuffyVec n rest)) where
  append = apBV

instance semigroupHotBufferPool :: Pos n => Semigroup (CfHotBufferPool MakeHotBufferPool (BuffyVec n Unit)) where
  append = apBV

instance semigroupSnappyBufferPool :: Pos n => Semigroup (CfSnappyBufferPool MakeSnappyBufferPool (BuffyVec n Unit)) where
  append = apBV

instance monoidBufferPool :: (Semigroup r, Pos n) => Monoid (ABufferPool n r) where
  mempty = makeBufferPool Nothing Nothing

instance monoidHotBufferPool :: (Pos n) => Monoid (AHotBufferPool n) where
  mempty = makeHotBufferPool { startsAt: 0.0, prevTime: 0.0 } Nothing Nothing

instance monoidSnappyBufferPool :: (Pos n) => Monoid (ASnappyBufferPool n) where
  mempty = makeSnappyBufferPool { startsAt: 0.0, prevTime: 0.0 } Nothing Nothing

bGain :: forall r. Maybe (Buffy r) -> AudioParameter
bGain = maybe (pure 0.0) (unwrap >>> _.gain)

bOnOff :: forall r. Maybe (Buffy r) -> APOnOff
bOnOff = maybe (pure Off) (unwrap >>> _.onOff)

instance actualizeBufferPool :: Actualize (ABufferPool n r) (SceneI a b) (Array { offset :: Number, rest :: r }) (CfBufferPool (MakeBufferPoolWithRest r) (BuffyVec n r)) where
  actualize (MakeBufferPoolWithRest r) (SceneI { time, headroom }) offsets = r { time, headroom: toNumber headroom / 1000.0, offsets }

instance actualizeHotBufferPool :: Actualize (AHotBufferPool n) (SceneI a b) Number (CfHotBufferPool MakeHotBufferPool (BuffyVec n Unit)) where
  actualize (MakeHotBufferPool r) (SceneI { time, headroom }) freq = r { time, headroom: toNumber headroom / 1000.0, freq }

instance actualizeSnappyBufferPool :: Actualize (ASnappyBufferPool n) (SceneI a b) { freq :: Number } (CfSnappyBufferPool MakeSnappyBufferPool (BuffyVec n Unit)) where
  actualize (MakeSnappyBufferPool r) (SceneI { time, headroom }) { freq } = r { time, headroom: toNumber headroom / 1000.0, freq }

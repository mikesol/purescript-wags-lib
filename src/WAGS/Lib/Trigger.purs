module WAGS.Lib.Trigger where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Lib.Blip (ABlip, MakeBlip(..), makeBlip)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Lib.Emitter (fEmitter)
import WAGS.Run (SceneI(..))

type TimeOffset
  = { time :: Number, offset :: Maybe Number }

type TimeHeadroomFreq
  = { time :: Number, headroom :: Number, freq :: Number }

newtype MakeTriggerWithOffset a
  = MakeTriggerWithOffset (TimeOffset -> a)

derive instance newtypeMakeTriggerWithOffset :: Newtype (MakeTriggerWithOffset a) _

derive instance functorMakeTriggerWithOffset :: Functor MakeTriggerWithOffset

derive newtype instance semigroupMakeTriggerWithOffset :: Semigroup a => Semigroup (MakeTriggerWithOffset a)

newtype MakeSnappyTrigger a
  = MakeSnappyTrigger (TimeHeadroomFreq -> a)

derive instance newtypeMakeSnappyTrigger :: Newtype (MakeSnappyTrigger a) _

derive instance functorMakeSnappyTrigger :: Functor MakeSnappyTrigger

derive newtype instance semigroupMakeSnappyTrigger :: Semigroup a => Semigroup (MakeSnappyTrigger a)

newtype CfTrigger f a
  = CfTrigger (Cofree f a)

type Time
  = Maybe Number

derive instance newtypeCfTrigger :: Newtype (CfTrigger MakeTriggerWithOffset Time) _

derive newtype instance functorCfTrigger :: Functor (CfTrigger MakeTriggerWithOffset)

derive newtype instance extendCfTrigger :: Extend (CfTrigger MakeTriggerWithOffset)

derive newtype instance comonadCfTrigger :: Comonad (CfTrigger MakeTriggerWithOffset)

derive newtype instance comonadCofreeCfTrigger :: ComonadCofree MakeTriggerWithOffset (CfTrigger MakeTriggerWithOffset)

type ATrigger
  = MakeTriggerWithOffset (CfTrigger MakeTriggerWithOffset Time)

newtype CfSnappyTrigger f a
  = CfSnappyTrigger (Cofree f a)

derive instance newtypeCfSnappyTrigger :: Newtype (CfSnappyTrigger MakeSnappyTrigger Time) _

derive newtype instance functorCfSnappyTrigger :: Functor (CfSnappyTrigger MakeSnappyTrigger)

derive newtype instance extendCfSnappyTrigger :: Extend (CfSnappyTrigger MakeSnappyTrigger)

derive newtype instance comonadCfSnappyTrigger :: Comonad (CfSnappyTrigger MakeSnappyTrigger)

derive newtype instance comonadCofreeCfSnappyTrigger :: ComonadCofree MakeSnappyTrigger (CfSnappyTrigger MakeSnappyTrigger)

type ASnappyTrigger
  = MakeSnappyTrigger (CfSnappyTrigger MakeSnappyTrigger Time)

makeTrigger :: ATrigger
makeTrigger = MakeTriggerWithOffset (go Nothing)
  where
  go :: Maybe Number -> TimeOffset -> CfTrigger MakeTriggerWithOffset Time
  go _ { time, offset: Just t } = let newT = time + t in wrap (Just (-t) :< map unwrap (wrap (go (Just newT))))

  go Nothing { offset: Nothing } = wrap (Nothing :< map unwrap (wrap (go Nothing)))

  go (Just t) { time, offset: Nothing } = let newT = time - t in wrap (Just newT :< map unwrap (wrap (go (Just t))))


makeSnappyTrigger :: ASnappyTrigger
makeSnappyTrigger = wrap (go blip trigger)
  where
  blip = makeBlip

  trigger = makeTrigger

  go :: ABlip -> ATrigger -> TimeHeadroomFreq -> CfSnappyTrigger MakeSnappyTrigger Time
  go (MakeBlip e) (MakeTriggerWithOffset b) { time, headroom, freq } = wrap (extract bnow :< map unwrap (wrap (go (unwrapCofree enow) (unwrapCofree bnow))))
    where
    emitted = fEmitter freq { time, headroom }

    enow = e (isJust emitted)

    bnow =
      b
        { time
        , offset:
            if (extract enow) then
              emitted
            else
              Nothing
        }

apBV ::
  forall w f.
  Newtype (w f Time) (Cofree f Time) =>
  Semigroup (f (w f Time)) =>
  Functor f =>
  ComonadCofree f (w f) =>
  Comonad (w f) =>
  w f Time -> w f Time -> w f Time
apBV i0r i1r =
  wrap
    ((extract i0r <|> extract i1r) :< map unwrap (unwrapCofree i0r <> unwrapCofree i1r))

instance semigroupTrigger :: Semigroup (CfTrigger MakeTriggerWithOffset Time) where
  append a b = apBV a b

instance semigroupSnappyTrigger :: Semigroup (CfSnappyTrigger MakeSnappyTrigger Time) where
  append a b = apBV a b

instance monoidTrigger :: Monoid ATrigger where
  mempty = makeTrigger

instance monoidSnappyTrigger :: Monoid ASnappyTrigger where
  mempty = makeSnappyTrigger

instance actualizeTrigger :: Actualize ATrigger (SceneI a b) (Maybe Number) (CfTrigger MakeTriggerWithOffset Time) where
  actualize (MakeTriggerWithOffset r) (SceneI { time }) offset = r { time, offset }

instance actualizeSnappyTrigger :: Actualize ASnappyTrigger (SceneI a b) Number (CfSnappyTrigger MakeSnappyTrigger Time) where
  actualize (MakeSnappyTrigger r) (SceneI { time, headroomInSeconds: headroom }) freq = r { time, headroom, freq }

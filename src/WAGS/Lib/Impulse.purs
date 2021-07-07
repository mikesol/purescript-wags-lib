module WAGS.Lib.Impulse where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree, head, tail, (:<))
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import WAGS.Lib.Cofree (class Actualize)

-- | A single impulse
makeImpulse :: AnImpulse
makeImpulse = wrap $ wrap $ go true
  where
  go tf = deferCofree \_ -> tf /\ pure (go false)

newtype AnImpulse
  = AnImpulse (Identity (Cofree Identity Boolean))

derive instance newtypeAnImpulse :: Newtype AnImpulse _

instance semigroupImpulse :: Semigroup AnImpulse where
  append (AnImpulse i0) (AnImpulse i1) =
    let
      i0r = unwrap i0

      i1r = unwrap i1
    in
      AnImpulse (wrap (deferCofree \_ -> ((head i0r) || (head i1r)) /\ (unwrap (append ((wrap (tail i0r)) :: AnImpulse) (wrap (tail i1r))))))

instance monoidImpulse :: Monoid AnImpulse where
  mempty = makeImpulse

instance actualizeImpulse :: Actualize AnImpulse e r (Cofree Identity Boolean) where
  actualize (AnImpulse (Identity c)) _ _ = c

-- | A stream that sends an impulse when moving from 0 to 1
makeBlip :: ABlip
makeBlip = wrap $ go false
  where
  go prev cur = (not prev && cur) :< go cur

newtype ABlip
  = ABlip (Boolean -> Cofree ((->) Boolean) Boolean)

derive instance newtypeABlip :: Newtype ABlip _

instance semigroupBlip :: Semigroup ABlip where
  append (ABlip i0) (ABlip i1) = ABlip f
    where
    f :: Boolean -> Cofree ((->) Boolean) Boolean
    f tho = ((head i0r) || (head i1r)) :< ((unwrap (append ((wrap (tail i0r)) :: ABlip) (wrap (tail i1r)))))
      where
      i0r = i0 tho

      i1r = i1 tho

instance monoidBlip :: Monoid ABlip where
  mempty = makeBlip

instance actualizeBlip :: Actualize ABlip e Boolean (Cofree ((->) Boolean) Boolean) where
  actualize (ABlip r) _ b = r b

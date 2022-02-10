module WAGS.Lib.Tidal.TLP where

import Prim.TypeError(class Fail, Text)
import Prim.Symbol (class Cons)

data Nat
foreign import data Z :: Nat
foreign import data S :: Nat -> Nat

class MiniNotation (sym :: Symbol)
instance miniNotationEmpty :: MiniNotation ""
else instance miniNotationInteresting :: (Cons head tail sym, AfterSpecialChar head tail Z Z) => MiniNotation sym
-- starting bloc AfterNominal
class AfterNominal (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterNominalaClean :: AfterNominal "a" "" Z Z
else instance afterNominalaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "a" "" Z (S n)
else instance afterNominalaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "a" "" (S n) x
else instance afterNominala :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "a" rest sq ltgt
else instance afterNominalbClean :: AfterNominal "b" "" Z Z
else instance afterNominalbFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "b" "" Z (S n)
else instance afterNominalbFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "b" "" (S n) x
else instance afterNominalb :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "b" rest sq ltgt
else instance afterNominalcClean :: AfterNominal "c" "" Z Z
else instance afterNominalcFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "c" "" Z (S n)
else instance afterNominalcFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "c" "" (S n) x
else instance afterNominalc :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "c" rest sq ltgt
else instance afterNominaldClean :: AfterNominal "d" "" Z Z
else instance afterNominaldFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "d" "" Z (S n)
else instance afterNominaldFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "d" "" (S n) x
else instance afterNominald :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "d" rest sq ltgt
else instance afterNominaleClean :: AfterNominal "e" "" Z Z
else instance afterNominaleFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "e" "" Z (S n)
else instance afterNominaleFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "e" "" (S n) x
else instance afterNominale :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "e" rest sq ltgt
else instance afterNominalfClean :: AfterNominal "f" "" Z Z
else instance afterNominalfFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "f" "" Z (S n)
else instance afterNominalfFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "f" "" (S n) x
else instance afterNominalf :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "f" rest sq ltgt
else instance afterNominalgClean :: AfterNominal "g" "" Z Z
else instance afterNominalgFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "g" "" Z (S n)
else instance afterNominalgFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "g" "" (S n) x
else instance afterNominalg :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "g" rest sq ltgt
else instance afterNominalhClean :: AfterNominal "h" "" Z Z
else instance afterNominalhFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "h" "" Z (S n)
else instance afterNominalhFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "h" "" (S n) x
else instance afterNominalh :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "h" rest sq ltgt
else instance afterNominaliClean :: AfterNominal "i" "" Z Z
else instance afterNominaliFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "i" "" Z (S n)
else instance afterNominaliFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "i" "" (S n) x
else instance afterNominali :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "i" rest sq ltgt
else instance afterNominaljClean :: AfterNominal "j" "" Z Z
else instance afterNominaljFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "j" "" Z (S n)
else instance afterNominaljFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "j" "" (S n) x
else instance afterNominalj :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "j" rest sq ltgt
else instance afterNominalkClean :: AfterNominal "k" "" Z Z
else instance afterNominalkFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "k" "" Z (S n)
else instance afterNominalkFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "k" "" (S n) x
else instance afterNominalk :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "k" rest sq ltgt
else instance afterNominallClean :: AfterNominal "l" "" Z Z
else instance afterNominallFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "l" "" Z (S n)
else instance afterNominallFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "l" "" (S n) x
else instance afterNominall :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "l" rest sq ltgt
else instance afterNominalmClean :: AfterNominal "m" "" Z Z
else instance afterNominalmFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "m" "" Z (S n)
else instance afterNominalmFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "m" "" (S n) x
else instance afterNominalm :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "m" rest sq ltgt
else instance afterNominalnClean :: AfterNominal "n" "" Z Z
else instance afterNominalnFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "n" "" Z (S n)
else instance afterNominalnFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "n" "" (S n) x
else instance afterNominaln :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "n" rest sq ltgt
else instance afterNominaloClean :: AfterNominal "o" "" Z Z
else instance afterNominaloFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "o" "" Z (S n)
else instance afterNominaloFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "o" "" (S n) x
else instance afterNominalo :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "o" rest sq ltgt
else instance afterNominalpClean :: AfterNominal "p" "" Z Z
else instance afterNominalpFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "p" "" Z (S n)
else instance afterNominalpFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "p" "" (S n) x
else instance afterNominalp :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "p" rest sq ltgt
else instance afterNominalqClean :: AfterNominal "q" "" Z Z
else instance afterNominalqFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "q" "" Z (S n)
else instance afterNominalqFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "q" "" (S n) x
else instance afterNominalq :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "q" rest sq ltgt
else instance afterNominalrClean :: AfterNominal "r" "" Z Z
else instance afterNominalrFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "r" "" Z (S n)
else instance afterNominalrFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "r" "" (S n) x
else instance afterNominalr :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "r" rest sq ltgt
else instance afterNominalsClean :: AfterNominal "s" "" Z Z
else instance afterNominalsFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "s" "" Z (S n)
else instance afterNominalsFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "s" "" (S n) x
else instance afterNominals :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "s" rest sq ltgt
else instance afterNominaltClean :: AfterNominal "t" "" Z Z
else instance afterNominaltFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "t" "" Z (S n)
else instance afterNominaltFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "t" "" (S n) x
else instance afterNominalt :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "t" rest sq ltgt
else instance afterNominaluClean :: AfterNominal "u" "" Z Z
else instance afterNominaluFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "u" "" Z (S n)
else instance afterNominaluFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "u" "" (S n) x
else instance afterNominalu :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "u" rest sq ltgt
else instance afterNominalvClean :: AfterNominal "v" "" Z Z
else instance afterNominalvFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "v" "" Z (S n)
else instance afterNominalvFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "v" "" (S n) x
else instance afterNominalv :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "v" rest sq ltgt
else instance afterNominalwClean :: AfterNominal "w" "" Z Z
else instance afterNominalwFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "w" "" Z (S n)
else instance afterNominalwFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "w" "" (S n) x
else instance afterNominalw :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "w" rest sq ltgt
else instance afterNominalxClean :: AfterNominal "x" "" Z Z
else instance afterNominalxFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "x" "" Z (S n)
else instance afterNominalxFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "x" "" (S n) x
else instance afterNominalx :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "x" rest sq ltgt
else instance afterNominalyClean :: AfterNominal "y" "" Z Z
else instance afterNominalyFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "y" "" Z (S n)
else instance afterNominalyFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "y" "" (S n) x
else instance afterNominaly :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "y" rest sq ltgt
else instance afterNominalzClean :: AfterNominal "z" "" Z Z
else instance afterNominalzFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "z" "" Z (S n)
else instance afterNominalzFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "z" "" (S n) x
else instance afterNominalz :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "z" rest sq ltgt
else instance afterNominalAClean :: AfterNominal "A" "" Z Z
else instance afterNominalAFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "A" "" Z (S n)
else instance afterNominalAFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "A" "" (S n) x
else instance afterNominalA :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "A" rest sq ltgt
else instance afterNominalBClean :: AfterNominal "B" "" Z Z
else instance afterNominalBFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "B" "" Z (S n)
else instance afterNominalBFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "B" "" (S n) x
else instance afterNominalB :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "B" rest sq ltgt
else instance afterNominalCClean :: AfterNominal "C" "" Z Z
else instance afterNominalCFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "C" "" Z (S n)
else instance afterNominalCFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "C" "" (S n) x
else instance afterNominalC :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "C" rest sq ltgt
else instance afterNominalDClean :: AfterNominal "D" "" Z Z
else instance afterNominalDFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "D" "" Z (S n)
else instance afterNominalDFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "D" "" (S n) x
else instance afterNominalD :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "D" rest sq ltgt
else instance afterNominalEClean :: AfterNominal "E" "" Z Z
else instance afterNominalEFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "E" "" Z (S n)
else instance afterNominalEFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "E" "" (S n) x
else instance afterNominalE :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "E" rest sq ltgt
else instance afterNominalFClean :: AfterNominal "F" "" Z Z
else instance afterNominalFFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "F" "" Z (S n)
else instance afterNominalFFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "F" "" (S n) x
else instance afterNominalF :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "F" rest sq ltgt
else instance afterNominalGClean :: AfterNominal "G" "" Z Z
else instance afterNominalGFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "G" "" Z (S n)
else instance afterNominalGFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "G" "" (S n) x
else instance afterNominalG :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "G" rest sq ltgt
else instance afterNominalHClean :: AfterNominal "H" "" Z Z
else instance afterNominalHFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "H" "" Z (S n)
else instance afterNominalHFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "H" "" (S n) x
else instance afterNominalH :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "H" rest sq ltgt
else instance afterNominalIClean :: AfterNominal "I" "" Z Z
else instance afterNominalIFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "I" "" Z (S n)
else instance afterNominalIFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "I" "" (S n) x
else instance afterNominalI :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "I" rest sq ltgt
else instance afterNominalJClean :: AfterNominal "J" "" Z Z
else instance afterNominalJFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "J" "" Z (S n)
else instance afterNominalJFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "J" "" (S n) x
else instance afterNominalJ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "J" rest sq ltgt
else instance afterNominalKClean :: AfterNominal "K" "" Z Z
else instance afterNominalKFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "K" "" Z (S n)
else instance afterNominalKFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "K" "" (S n) x
else instance afterNominalK :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "K" rest sq ltgt
else instance afterNominalLClean :: AfterNominal "L" "" Z Z
else instance afterNominalLFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "L" "" Z (S n)
else instance afterNominalLFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "L" "" (S n) x
else instance afterNominalL :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "L" rest sq ltgt
else instance afterNominalMClean :: AfterNominal "M" "" Z Z
else instance afterNominalMFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "M" "" Z (S n)
else instance afterNominalMFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "M" "" (S n) x
else instance afterNominalM :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "M" rest sq ltgt
else instance afterNominalNClean :: AfterNominal "N" "" Z Z
else instance afterNominalNFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "N" "" Z (S n)
else instance afterNominalNFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "N" "" (S n) x
else instance afterNominalN :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "N" rest sq ltgt
else instance afterNominalOClean :: AfterNominal "O" "" Z Z
else instance afterNominalOFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "O" "" Z (S n)
else instance afterNominalOFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "O" "" (S n) x
else instance afterNominalO :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "O" rest sq ltgt
else instance afterNominalPClean :: AfterNominal "P" "" Z Z
else instance afterNominalPFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "P" "" Z (S n)
else instance afterNominalPFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "P" "" (S n) x
else instance afterNominalP :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "P" rest sq ltgt
else instance afterNominalQClean :: AfterNominal "Q" "" Z Z
else instance afterNominalQFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "Q" "" Z (S n)
else instance afterNominalQFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "Q" "" (S n) x
else instance afterNominalQ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "Q" rest sq ltgt
else instance afterNominalRClean :: AfterNominal "R" "" Z Z
else instance afterNominalRFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "R" "" Z (S n)
else instance afterNominalRFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "R" "" (S n) x
else instance afterNominalR :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "R" rest sq ltgt
else instance afterNominalSClean :: AfterNominal "S" "" Z Z
else instance afterNominalSFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "S" "" Z (S n)
else instance afterNominalSFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "S" "" (S n) x
else instance afterNominalS :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "S" rest sq ltgt
else instance afterNominalTClean :: AfterNominal "T" "" Z Z
else instance afterNominalTFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "T" "" Z (S n)
else instance afterNominalTFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "T" "" (S n) x
else instance afterNominalT :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "T" rest sq ltgt
else instance afterNominalUClean :: AfterNominal "U" "" Z Z
else instance afterNominalUFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "U" "" Z (S n)
else instance afterNominalUFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "U" "" (S n) x
else instance afterNominalU :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "U" rest sq ltgt
else instance afterNominalVClean :: AfterNominal "V" "" Z Z
else instance afterNominalVFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "V" "" Z (S n)
else instance afterNominalVFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "V" "" (S n) x
else instance afterNominalV :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "V" rest sq ltgt
else instance afterNominalWClean :: AfterNominal "W" "" Z Z
else instance afterNominalWFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "W" "" Z (S n)
else instance afterNominalWFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "W" "" (S n) x
else instance afterNominalW :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "W" rest sq ltgt
else instance afterNominalXClean :: AfterNominal "X" "" Z Z
else instance afterNominalXFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "X" "" Z (S n)
else instance afterNominalXFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "X" "" (S n) x
else instance afterNominalX :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "X" rest sq ltgt
else instance afterNominalYClean :: AfterNominal "Y" "" Z Z
else instance afterNominalYFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "Y" "" Z (S n)
else instance afterNominalYFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "Y" "" (S n) x
else instance afterNominalY :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "Y" rest sq ltgt
else instance afterNominalZClean :: AfterNominal "Z" "" Z Z
else instance afterNominalZFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "Z" "" Z (S n)
else instance afterNominalZFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "Z" "" (S n) x
else instance afterNominalZ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "Z" rest sq ltgt
else instance afterNominal0Clean :: AfterNominal "0" "" Z Z
else instance afterNominal0FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "0" "" Z (S n)
else instance afterNominal0FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "0" "" (S n) x
else instance afterNominal0 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "0" rest sq ltgt
else instance afterNominal1Clean :: AfterNominal "1" "" Z Z
else instance afterNominal1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "1" "" Z (S n)
else instance afterNominal1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "1" "" (S n) x
else instance afterNominal1 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "1" rest sq ltgt
else instance afterNominal2Clean :: AfterNominal "2" "" Z Z
else instance afterNominal2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "2" "" Z (S n)
else instance afterNominal2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "2" "" (S n) x
else instance afterNominal2 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "2" rest sq ltgt
else instance afterNominal3Clean :: AfterNominal "3" "" Z Z
else instance afterNominal3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "3" "" Z (S n)
else instance afterNominal3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "3" "" (S n) x
else instance afterNominal3 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "3" rest sq ltgt
else instance afterNominal4Clean :: AfterNominal "4" "" Z Z
else instance afterNominal4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "4" "" Z (S n)
else instance afterNominal4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "4" "" (S n) x
else instance afterNominal4 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "4" rest sq ltgt
else instance afterNominal5Clean :: AfterNominal "5" "" Z Z
else instance afterNominal5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "5" "" Z (S n)
else instance afterNominal5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "5" "" (S n) x
else instance afterNominal5 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "5" rest sq ltgt
else instance afterNominal6Clean :: AfterNominal "6" "" Z Z
else instance afterNominal6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "6" "" Z (S n)
else instance afterNominal6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "6" "" (S n) x
else instance afterNominal6 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "6" rest sq ltgt
else instance afterNominal7Clean :: AfterNominal "7" "" Z Z
else instance afterNominal7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "7" "" Z (S n)
else instance afterNominal7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "7" "" (S n) x
else instance afterNominal7 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "7" rest sq ltgt
else instance afterNominal8Clean :: AfterNominal "8" "" Z Z
else instance afterNominal8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "8" "" Z (S n)
else instance afterNominal8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "8" "" (S n) x
else instance afterNominal8 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "8" rest sq ltgt
else instance afterNominal9Clean :: AfterNominal "9" "" Z Z
else instance afterNominal9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "9" "" Z (S n)
else instance afterNominal9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "9" "" (S n) x
else instance afterNominal9 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "9" rest sq ltgt
else instance afterNominalColonClean :: AfterNominal ":" "" Z Z
else instance afterNominalColonFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal ":" "" Z (S n)
else instance afterNominalColonFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal ":" "" (S n) x
else instance afterNominalColon :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal ":" rest sq ltgt
else instance afterNominalTildeClean :: AfterNominal "~" "" Z Z
else instance afterNominalTildeFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "~" "" Z (S n)
else instance afterNominalTildeFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "~" "" (S n) x
else instance afterNominalTilde :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterNominal "~" rest sq ltgt
else instance afterNominalSpaceClean :: AfterNominal " " "" Z Z
else instance afterNominalSpaceFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal " " "" Z (S n)
else instance afterNominalSpaceFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal " " "" (S n) x
else instance afterNominalSpace :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal " " rest sq ltgt
else instance afterNominalTabClean :: AfterNominal "\t" "" Z Z
else instance afterNominalTabFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "\t" "" Z (S n)
else instance afterNominalTabFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "\t" "" (S n) x
else instance afterNominalTab :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal "\t" rest sq ltgt
else instance afterNominalNewLineClean :: AfterNominal "\n" "" Z Z
else instance afterNominalNewLineFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "\n" "" Z (S n)
else instance afterNominalNewLineFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "\n" "" (S n) x
else instance afterNominalNewLine :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal "\n" rest sq ltgt
else instance afterNominalCommaClean :: AfterNominal "," "" Z Z
else instance afterNominalCommaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "," "" Z (S n)
else instance afterNominalCommaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "," "" (S n) x
else instance afterNominalComma :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal "," rest sq ltgt
else instance afterNominalStarClean :: AfterNominal "*" "" Z Z
else instance afterNominalStarFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "*" "" Z (S n)
else instance afterNominalStarFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "*" "" (S n) x
else instance afterNominalStar :: (Cons head tail rest, AfterMult head tail sq ltgt) => AfterNominal "*" rest sq ltgt
else instance afterNominalSemiColonClean :: AfterNominal ";" "" Z Z
else instance afterNominalSemiColonFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal ";" "" Z (S n)
else instance afterNominalSemiColonFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal ";" "" (S n) x
else instance afterNominalSemiColon :: (Cons head tail rest, AfterTag head tail sq ltgt) => AfterNominal ";" rest sq ltgt
else instance afterNominalOpenSquareClean :: AfterNominal "[" "" Z Z
else instance afterNominalOpenSquareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "[" "" Z (S n)
else instance afterNominalOpenSquareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "[" "" (S n) x
else instance afterNominalOpenSquare :: (Cons head tail rest, AfterSpecialChar head tail (S sq) ltgt) => AfterNominal "[" rest sq ltgt
else instance afterNominalLessThanClean :: AfterNominal "<" "" Z Z
else instance afterNominalLessThanFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterNominal "<" "" Z (S n)
else instance afterNominalLessThanFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterNominal "<" "" (S n) x
else instance afterNominalLessThan :: (Cons head tail rest, AfterSpecialChar head tail sq (S ltgt)) => AfterNominal "<" rest sq ltgt
else instance afterNominalValidGt :: AfterNominal ">" "" Z (S Z)
else instance afterNominalValidSq :: AfterNominal "]" "" (S Z) Z
else instance afterNominalTooManyGt :: Fail (Text "Too many closing >") => AfterNominal ">" "" Z Z
else instance afterNominalTooManySq :: Fail (Text "Too many closing ]") => AfterNominal "]" "" Z Z
else instance afterNominalNotEnoughGt :: Fail (Text "Not enough closing >") => AfterNominal ">" "" Z (S ltgt)
else instance afterNominalNotEnoughSq :: Fail (Text "Not enough closing ]") => AfterNominal "]" "" (S sq) Z
else instance afterNominalContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal ">" rest sq (S ltgt)
else instance afterNominalContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterNominal "]" rest (S sq) ltgt
else instance afterNominalUnderfullGt :: Fail (Text "Too many closing >") => AfterNominal ">" rest sq Z
else instance afterNominalUnderfullSq :: Fail (Text "Too many closing ]") => AfterNominal "]" rest Z ltgt
else instance afterNominalSpurious :: Fail (Text "Not a valid symbol") => AfterNominal invalid rest Z Z
-- starting bloc AfterSpecialChar
class AfterSpecialChar (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterSpecialCharaClean :: AfterSpecialChar "a" "" Z Z
else instance afterSpecialCharaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "a" "" Z (S n)
else instance afterSpecialCharaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "a" "" (S n) x
else instance afterSpecialChara :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "a" rest sq ltgt
else instance afterSpecialCharbClean :: AfterSpecialChar "b" "" Z Z
else instance afterSpecialCharbFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "b" "" Z (S n)
else instance afterSpecialCharbFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "b" "" (S n) x
else instance afterSpecialCharb :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "b" rest sq ltgt
else instance afterSpecialCharcClean :: AfterSpecialChar "c" "" Z Z
else instance afterSpecialCharcFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "c" "" Z (S n)
else instance afterSpecialCharcFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "c" "" (S n) x
else instance afterSpecialCharc :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "c" rest sq ltgt
else instance afterSpecialChardClean :: AfterSpecialChar "d" "" Z Z
else instance afterSpecialChardFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "d" "" Z (S n)
else instance afterSpecialChardFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "d" "" (S n) x
else instance afterSpecialChard :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "d" rest sq ltgt
else instance afterSpecialChareClean :: AfterSpecialChar "e" "" Z Z
else instance afterSpecialChareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "e" "" Z (S n)
else instance afterSpecialChareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "e" "" (S n) x
else instance afterSpecialChare :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "e" rest sq ltgt
else instance afterSpecialCharfClean :: AfterSpecialChar "f" "" Z Z
else instance afterSpecialCharfFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "f" "" Z (S n)
else instance afterSpecialCharfFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "f" "" (S n) x
else instance afterSpecialCharf :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "f" rest sq ltgt
else instance afterSpecialChargClean :: AfterSpecialChar "g" "" Z Z
else instance afterSpecialChargFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "g" "" Z (S n)
else instance afterSpecialChargFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "g" "" (S n) x
else instance afterSpecialCharg :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "g" rest sq ltgt
else instance afterSpecialCharhClean :: AfterSpecialChar "h" "" Z Z
else instance afterSpecialCharhFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "h" "" Z (S n)
else instance afterSpecialCharhFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "h" "" (S n) x
else instance afterSpecialCharh :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "h" rest sq ltgt
else instance afterSpecialChariClean :: AfterSpecialChar "i" "" Z Z
else instance afterSpecialChariFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "i" "" Z (S n)
else instance afterSpecialChariFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "i" "" (S n) x
else instance afterSpecialChari :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "i" rest sq ltgt
else instance afterSpecialCharjClean :: AfterSpecialChar "j" "" Z Z
else instance afterSpecialCharjFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "j" "" Z (S n)
else instance afterSpecialCharjFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "j" "" (S n) x
else instance afterSpecialCharj :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "j" rest sq ltgt
else instance afterSpecialCharkClean :: AfterSpecialChar "k" "" Z Z
else instance afterSpecialCharkFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "k" "" Z (S n)
else instance afterSpecialCharkFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "k" "" (S n) x
else instance afterSpecialChark :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "k" rest sq ltgt
else instance afterSpecialCharlClean :: AfterSpecialChar "l" "" Z Z
else instance afterSpecialCharlFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "l" "" Z (S n)
else instance afterSpecialCharlFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "l" "" (S n) x
else instance afterSpecialCharl :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "l" rest sq ltgt
else instance afterSpecialCharmClean :: AfterSpecialChar "m" "" Z Z
else instance afterSpecialCharmFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "m" "" Z (S n)
else instance afterSpecialCharmFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "m" "" (S n) x
else instance afterSpecialCharm :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "m" rest sq ltgt
else instance afterSpecialCharnClean :: AfterSpecialChar "n" "" Z Z
else instance afterSpecialCharnFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "n" "" Z (S n)
else instance afterSpecialCharnFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "n" "" (S n) x
else instance afterSpecialCharn :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "n" rest sq ltgt
else instance afterSpecialCharoClean :: AfterSpecialChar "o" "" Z Z
else instance afterSpecialCharoFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "o" "" Z (S n)
else instance afterSpecialCharoFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "o" "" (S n) x
else instance afterSpecialCharo :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "o" rest sq ltgt
else instance afterSpecialCharpClean :: AfterSpecialChar "p" "" Z Z
else instance afterSpecialCharpFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "p" "" Z (S n)
else instance afterSpecialCharpFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "p" "" (S n) x
else instance afterSpecialCharp :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "p" rest sq ltgt
else instance afterSpecialCharqClean :: AfterSpecialChar "q" "" Z Z
else instance afterSpecialCharqFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "q" "" Z (S n)
else instance afterSpecialCharqFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "q" "" (S n) x
else instance afterSpecialCharq :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "q" rest sq ltgt
else instance afterSpecialCharrClean :: AfterSpecialChar "r" "" Z Z
else instance afterSpecialCharrFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "r" "" Z (S n)
else instance afterSpecialCharrFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "r" "" (S n) x
else instance afterSpecialCharr :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "r" rest sq ltgt
else instance afterSpecialCharsClean :: AfterSpecialChar "s" "" Z Z
else instance afterSpecialCharsFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "s" "" Z (S n)
else instance afterSpecialCharsFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "s" "" (S n) x
else instance afterSpecialChars :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "s" rest sq ltgt
else instance afterSpecialChartClean :: AfterSpecialChar "t" "" Z Z
else instance afterSpecialChartFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "t" "" Z (S n)
else instance afterSpecialChartFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "t" "" (S n) x
else instance afterSpecialChart :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "t" rest sq ltgt
else instance afterSpecialCharuClean :: AfterSpecialChar "u" "" Z Z
else instance afterSpecialCharuFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "u" "" Z (S n)
else instance afterSpecialCharuFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "u" "" (S n) x
else instance afterSpecialCharu :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "u" rest sq ltgt
else instance afterSpecialCharvClean :: AfterSpecialChar "v" "" Z Z
else instance afterSpecialCharvFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "v" "" Z (S n)
else instance afterSpecialCharvFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "v" "" (S n) x
else instance afterSpecialCharv :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "v" rest sq ltgt
else instance afterSpecialCharwClean :: AfterSpecialChar "w" "" Z Z
else instance afterSpecialCharwFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "w" "" Z (S n)
else instance afterSpecialCharwFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "w" "" (S n) x
else instance afterSpecialCharw :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "w" rest sq ltgt
else instance afterSpecialCharxClean :: AfterSpecialChar "x" "" Z Z
else instance afterSpecialCharxFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "x" "" Z (S n)
else instance afterSpecialCharxFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "x" "" (S n) x
else instance afterSpecialCharx :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "x" rest sq ltgt
else instance afterSpecialCharyClean :: AfterSpecialChar "y" "" Z Z
else instance afterSpecialCharyFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "y" "" Z (S n)
else instance afterSpecialCharyFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "y" "" (S n) x
else instance afterSpecialChary :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "y" rest sq ltgt
else instance afterSpecialCharzClean :: AfterSpecialChar "z" "" Z Z
else instance afterSpecialCharzFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "z" "" Z (S n)
else instance afterSpecialCharzFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "z" "" (S n) x
else instance afterSpecialCharz :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "z" rest sq ltgt
else instance afterSpecialCharAClean :: AfterSpecialChar "A" "" Z Z
else instance afterSpecialCharAFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "A" "" Z (S n)
else instance afterSpecialCharAFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "A" "" (S n) x
else instance afterSpecialCharA :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "A" rest sq ltgt
else instance afterSpecialCharBClean :: AfterSpecialChar "B" "" Z Z
else instance afterSpecialCharBFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "B" "" Z (S n)
else instance afterSpecialCharBFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "B" "" (S n) x
else instance afterSpecialCharB :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "B" rest sq ltgt
else instance afterSpecialCharCClean :: AfterSpecialChar "C" "" Z Z
else instance afterSpecialCharCFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "C" "" Z (S n)
else instance afterSpecialCharCFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "C" "" (S n) x
else instance afterSpecialCharC :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "C" rest sq ltgt
else instance afterSpecialCharDClean :: AfterSpecialChar "D" "" Z Z
else instance afterSpecialCharDFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "D" "" Z (S n)
else instance afterSpecialCharDFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "D" "" (S n) x
else instance afterSpecialCharD :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "D" rest sq ltgt
else instance afterSpecialCharEClean :: AfterSpecialChar "E" "" Z Z
else instance afterSpecialCharEFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "E" "" Z (S n)
else instance afterSpecialCharEFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "E" "" (S n) x
else instance afterSpecialCharE :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "E" rest sq ltgt
else instance afterSpecialCharFClean :: AfterSpecialChar "F" "" Z Z
else instance afterSpecialCharFFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "F" "" Z (S n)
else instance afterSpecialCharFFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "F" "" (S n) x
else instance afterSpecialCharF :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "F" rest sq ltgt
else instance afterSpecialCharGClean :: AfterSpecialChar "G" "" Z Z
else instance afterSpecialCharGFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "G" "" Z (S n)
else instance afterSpecialCharGFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "G" "" (S n) x
else instance afterSpecialCharG :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "G" rest sq ltgt
else instance afterSpecialCharHClean :: AfterSpecialChar "H" "" Z Z
else instance afterSpecialCharHFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "H" "" Z (S n)
else instance afterSpecialCharHFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "H" "" (S n) x
else instance afterSpecialCharH :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "H" rest sq ltgt
else instance afterSpecialCharIClean :: AfterSpecialChar "I" "" Z Z
else instance afterSpecialCharIFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "I" "" Z (S n)
else instance afterSpecialCharIFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "I" "" (S n) x
else instance afterSpecialCharI :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "I" rest sq ltgt
else instance afterSpecialCharJClean :: AfterSpecialChar "J" "" Z Z
else instance afterSpecialCharJFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "J" "" Z (S n)
else instance afterSpecialCharJFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "J" "" (S n) x
else instance afterSpecialCharJ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "J" rest sq ltgt
else instance afterSpecialCharKClean :: AfterSpecialChar "K" "" Z Z
else instance afterSpecialCharKFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "K" "" Z (S n)
else instance afterSpecialCharKFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "K" "" (S n) x
else instance afterSpecialCharK :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "K" rest sq ltgt
else instance afterSpecialCharLClean :: AfterSpecialChar "L" "" Z Z
else instance afterSpecialCharLFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "L" "" Z (S n)
else instance afterSpecialCharLFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "L" "" (S n) x
else instance afterSpecialCharL :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "L" rest sq ltgt
else instance afterSpecialCharMClean :: AfterSpecialChar "M" "" Z Z
else instance afterSpecialCharMFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "M" "" Z (S n)
else instance afterSpecialCharMFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "M" "" (S n) x
else instance afterSpecialCharM :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "M" rest sq ltgt
else instance afterSpecialCharNClean :: AfterSpecialChar "N" "" Z Z
else instance afterSpecialCharNFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "N" "" Z (S n)
else instance afterSpecialCharNFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "N" "" (S n) x
else instance afterSpecialCharN :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "N" rest sq ltgt
else instance afterSpecialCharOClean :: AfterSpecialChar "O" "" Z Z
else instance afterSpecialCharOFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "O" "" Z (S n)
else instance afterSpecialCharOFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "O" "" (S n) x
else instance afterSpecialCharO :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "O" rest sq ltgt
else instance afterSpecialCharPClean :: AfterSpecialChar "P" "" Z Z
else instance afterSpecialCharPFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "P" "" Z (S n)
else instance afterSpecialCharPFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "P" "" (S n) x
else instance afterSpecialCharP :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "P" rest sq ltgt
else instance afterSpecialCharQClean :: AfterSpecialChar "Q" "" Z Z
else instance afterSpecialCharQFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "Q" "" Z (S n)
else instance afterSpecialCharQFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "Q" "" (S n) x
else instance afterSpecialCharQ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "Q" rest sq ltgt
else instance afterSpecialCharRClean :: AfterSpecialChar "R" "" Z Z
else instance afterSpecialCharRFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "R" "" Z (S n)
else instance afterSpecialCharRFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "R" "" (S n) x
else instance afterSpecialCharR :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "R" rest sq ltgt
else instance afterSpecialCharSClean :: AfterSpecialChar "S" "" Z Z
else instance afterSpecialCharSFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "S" "" Z (S n)
else instance afterSpecialCharSFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "S" "" (S n) x
else instance afterSpecialCharS :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "S" rest sq ltgt
else instance afterSpecialCharTClean :: AfterSpecialChar "T" "" Z Z
else instance afterSpecialCharTFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "T" "" Z (S n)
else instance afterSpecialCharTFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "T" "" (S n) x
else instance afterSpecialCharT :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "T" rest sq ltgt
else instance afterSpecialCharUClean :: AfterSpecialChar "U" "" Z Z
else instance afterSpecialCharUFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "U" "" Z (S n)
else instance afterSpecialCharUFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "U" "" (S n) x
else instance afterSpecialCharU :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "U" rest sq ltgt
else instance afterSpecialCharVClean :: AfterSpecialChar "V" "" Z Z
else instance afterSpecialCharVFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "V" "" Z (S n)
else instance afterSpecialCharVFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "V" "" (S n) x
else instance afterSpecialCharV :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "V" rest sq ltgt
else instance afterSpecialCharWClean :: AfterSpecialChar "W" "" Z Z
else instance afterSpecialCharWFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "W" "" Z (S n)
else instance afterSpecialCharWFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "W" "" (S n) x
else instance afterSpecialCharW :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "W" rest sq ltgt
else instance afterSpecialCharXClean :: AfterSpecialChar "X" "" Z Z
else instance afterSpecialCharXFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "X" "" Z (S n)
else instance afterSpecialCharXFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "X" "" (S n) x
else instance afterSpecialCharX :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "X" rest sq ltgt
else instance afterSpecialCharYClean :: AfterSpecialChar "Y" "" Z Z
else instance afterSpecialCharYFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "Y" "" Z (S n)
else instance afterSpecialCharYFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "Y" "" (S n) x
else instance afterSpecialCharY :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "Y" rest sq ltgt
else instance afterSpecialCharZClean :: AfterSpecialChar "Z" "" Z Z
else instance afterSpecialCharZFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "Z" "" Z (S n)
else instance afterSpecialCharZFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "Z" "" (S n) x
else instance afterSpecialCharZ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "Z" rest sq ltgt
else instance afterSpecialChar0Clean :: AfterSpecialChar "0" "" Z Z
else instance afterSpecialChar0FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "0" "" Z (S n)
else instance afterSpecialChar0FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "0" "" (S n) x
else instance afterSpecialChar0 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "0" rest sq ltgt
else instance afterSpecialChar1Clean :: AfterSpecialChar "1" "" Z Z
else instance afterSpecialChar1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "1" "" Z (S n)
else instance afterSpecialChar1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "1" "" (S n) x
else instance afterSpecialChar1 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "1" rest sq ltgt
else instance afterSpecialChar2Clean :: AfterSpecialChar "2" "" Z Z
else instance afterSpecialChar2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "2" "" Z (S n)
else instance afterSpecialChar2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "2" "" (S n) x
else instance afterSpecialChar2 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "2" rest sq ltgt
else instance afterSpecialChar3Clean :: AfterSpecialChar "3" "" Z Z
else instance afterSpecialChar3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "3" "" Z (S n)
else instance afterSpecialChar3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "3" "" (S n) x
else instance afterSpecialChar3 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "3" rest sq ltgt
else instance afterSpecialChar4Clean :: AfterSpecialChar "4" "" Z Z
else instance afterSpecialChar4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "4" "" Z (S n)
else instance afterSpecialChar4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "4" "" (S n) x
else instance afterSpecialChar4 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "4" rest sq ltgt
else instance afterSpecialChar5Clean :: AfterSpecialChar "5" "" Z Z
else instance afterSpecialChar5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "5" "" Z (S n)
else instance afterSpecialChar5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "5" "" (S n) x
else instance afterSpecialChar5 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "5" rest sq ltgt
else instance afterSpecialChar6Clean :: AfterSpecialChar "6" "" Z Z
else instance afterSpecialChar6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "6" "" Z (S n)
else instance afterSpecialChar6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "6" "" (S n) x
else instance afterSpecialChar6 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "6" rest sq ltgt
else instance afterSpecialChar7Clean :: AfterSpecialChar "7" "" Z Z
else instance afterSpecialChar7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "7" "" Z (S n)
else instance afterSpecialChar7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "7" "" (S n) x
else instance afterSpecialChar7 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "7" rest sq ltgt
else instance afterSpecialChar8Clean :: AfterSpecialChar "8" "" Z Z
else instance afterSpecialChar8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "8" "" Z (S n)
else instance afterSpecialChar8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "8" "" (S n) x
else instance afterSpecialChar8 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "8" rest sq ltgt
else instance afterSpecialChar9Clean :: AfterSpecialChar "9" "" Z Z
else instance afterSpecialChar9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "9" "" Z (S n)
else instance afterSpecialChar9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "9" "" (S n) x
else instance afterSpecialChar9 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "9" rest sq ltgt
else instance afterSpecialCharColonClean :: AfterSpecialChar ":" "" Z Z
else instance afterSpecialCharColonFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar ":" "" Z (S n)
else instance afterSpecialCharColonFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar ":" "" (S n) x
else instance afterSpecialCharColon :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar ":" rest sq ltgt
else instance afterSpecialCharTildeClean :: AfterSpecialChar "~" "" Z Z
else instance afterSpecialCharTildeFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "~" "" Z (S n)
else instance afterSpecialCharTildeFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "~" "" (S n) x
else instance afterSpecialCharTilde :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterSpecialChar "~" rest sq ltgt
else instance afterSpecialCharSpaceClean :: AfterSpecialChar " " "" Z Z
else instance afterSpecialCharSpaceFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar " " "" Z (S n)
else instance afterSpecialCharSpaceFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar " " "" (S n) x
else instance afterSpecialCharSpace :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar " " rest sq ltgt
else instance afterSpecialCharTabClean :: AfterSpecialChar "\t" "" Z Z
else instance afterSpecialCharTabFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "\t" "" Z (S n)
else instance afterSpecialCharTabFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "\t" "" (S n) x
else instance afterSpecialCharTab :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar "\t" rest sq ltgt
else instance afterSpecialCharNewLineClean :: AfterSpecialChar "\n" "" Z Z
else instance afterSpecialCharNewLineFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "\n" "" Z (S n)
else instance afterSpecialCharNewLineFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "\n" "" (S n) x
else instance afterSpecialCharNewLine :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar "\n" rest sq ltgt
else instance afterSpecialCharCommaClean :: AfterSpecialChar "," "" Z Z
else instance afterSpecialCharCommaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "," "" Z (S n)
else instance afterSpecialCharCommaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "," "" (S n) x
else instance afterSpecialCharComma :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar "," rest sq ltgt
else instance afterSpecialCharOpenSquareClean :: AfterSpecialChar "[" "" Z Z
else instance afterSpecialCharOpenSquareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "[" "" Z (S n)
else instance afterSpecialCharOpenSquareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "[" "" (S n) x
else instance afterSpecialCharOpenSquare :: (Cons head tail rest, AfterSpecialChar head tail (S sq) ltgt) => AfterSpecialChar "[" rest sq ltgt
else instance afterSpecialCharLessThanClean :: AfterSpecialChar "<" "" Z Z
else instance afterSpecialCharLessThanFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSpecialChar "<" "" Z (S n)
else instance afterSpecialCharLessThanFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSpecialChar "<" "" (S n) x
else instance afterSpecialCharLessThan :: (Cons head tail rest, AfterSpecialChar head tail sq (S ltgt)) => AfterSpecialChar "<" rest sq ltgt
else instance afterSpecialCharValidGt :: AfterSpecialChar ">" "" Z (S Z)
else instance afterSpecialCharValidSq :: AfterSpecialChar "]" "" (S Z) Z
else instance afterSpecialCharTooManyGt :: Fail (Text "Too many closing >") => AfterSpecialChar ">" "" Z Z
else instance afterSpecialCharTooManySq :: Fail (Text "Too many closing ]") => AfterSpecialChar "]" "" Z Z
else instance afterSpecialCharNotEnoughGt :: Fail (Text "Not enough closing >") => AfterSpecialChar ">" "" Z (S ltgt)
else instance afterSpecialCharNotEnoughSq :: Fail (Text "Not enough closing ]") => AfterSpecialChar "]" "" (S sq) Z
else instance afterSpecialCharContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar ">" rest sq (S ltgt)
else instance afterSpecialCharContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSpecialChar "]" rest (S sq) ltgt
else instance afterSpecialCharUnderfullGt :: Fail (Text "Too many closing >") => AfterSpecialChar ">" rest sq Z
else instance afterSpecialCharUnderfullSq :: Fail (Text "Too many closing ]") => AfterSpecialChar "]" rest Z ltgt
else instance afterSpecialCharSpurious :: Fail (Text "Not a valid symbol") => AfterSpecialChar invalid rest Z Z
-- starting bloc AfterTag
class AfterTag (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterTagaClean :: AfterTag "a" "" Z Z
else instance afterTagaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "a" "" Z (S n)
else instance afterTagaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "a" "" (S n) x
else instance afterTaga :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "a" rest sq ltgt
else instance afterTagbClean :: AfterTag "b" "" Z Z
else instance afterTagbFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "b" "" Z (S n)
else instance afterTagbFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "b" "" (S n) x
else instance afterTagb :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "b" rest sq ltgt
else instance afterTagcClean :: AfterTag "c" "" Z Z
else instance afterTagcFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "c" "" Z (S n)
else instance afterTagcFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "c" "" (S n) x
else instance afterTagc :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "c" rest sq ltgt
else instance afterTagdClean :: AfterTag "d" "" Z Z
else instance afterTagdFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "d" "" Z (S n)
else instance afterTagdFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "d" "" (S n) x
else instance afterTagd :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "d" rest sq ltgt
else instance afterTageClean :: AfterTag "e" "" Z Z
else instance afterTageFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "e" "" Z (S n)
else instance afterTageFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "e" "" (S n) x
else instance afterTage :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "e" rest sq ltgt
else instance afterTagfClean :: AfterTag "f" "" Z Z
else instance afterTagfFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "f" "" Z (S n)
else instance afterTagfFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "f" "" (S n) x
else instance afterTagf :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "f" rest sq ltgt
else instance afterTaggClean :: AfterTag "g" "" Z Z
else instance afterTaggFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "g" "" Z (S n)
else instance afterTaggFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "g" "" (S n) x
else instance afterTagg :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "g" rest sq ltgt
else instance afterTaghClean :: AfterTag "h" "" Z Z
else instance afterTaghFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "h" "" Z (S n)
else instance afterTaghFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "h" "" (S n) x
else instance afterTagh :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "h" rest sq ltgt
else instance afterTagiClean :: AfterTag "i" "" Z Z
else instance afterTagiFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "i" "" Z (S n)
else instance afterTagiFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "i" "" (S n) x
else instance afterTagi :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "i" rest sq ltgt
else instance afterTagjClean :: AfterTag "j" "" Z Z
else instance afterTagjFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "j" "" Z (S n)
else instance afterTagjFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "j" "" (S n) x
else instance afterTagj :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "j" rest sq ltgt
else instance afterTagkClean :: AfterTag "k" "" Z Z
else instance afterTagkFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "k" "" Z (S n)
else instance afterTagkFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "k" "" (S n) x
else instance afterTagk :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "k" rest sq ltgt
else instance afterTaglClean :: AfterTag "l" "" Z Z
else instance afterTaglFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "l" "" Z (S n)
else instance afterTaglFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "l" "" (S n) x
else instance afterTagl :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "l" rest sq ltgt
else instance afterTagmClean :: AfterTag "m" "" Z Z
else instance afterTagmFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "m" "" Z (S n)
else instance afterTagmFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "m" "" (S n) x
else instance afterTagm :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "m" rest sq ltgt
else instance afterTagnClean :: AfterTag "n" "" Z Z
else instance afterTagnFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "n" "" Z (S n)
else instance afterTagnFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "n" "" (S n) x
else instance afterTagn :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "n" rest sq ltgt
else instance afterTagoClean :: AfterTag "o" "" Z Z
else instance afterTagoFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "o" "" Z (S n)
else instance afterTagoFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "o" "" (S n) x
else instance afterTago :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "o" rest sq ltgt
else instance afterTagpClean :: AfterTag "p" "" Z Z
else instance afterTagpFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "p" "" Z (S n)
else instance afterTagpFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "p" "" (S n) x
else instance afterTagp :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "p" rest sq ltgt
else instance afterTagqClean :: AfterTag "q" "" Z Z
else instance afterTagqFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "q" "" Z (S n)
else instance afterTagqFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "q" "" (S n) x
else instance afterTagq :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "q" rest sq ltgt
else instance afterTagrClean :: AfterTag "r" "" Z Z
else instance afterTagrFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "r" "" Z (S n)
else instance afterTagrFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "r" "" (S n) x
else instance afterTagr :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "r" rest sq ltgt
else instance afterTagsClean :: AfterTag "s" "" Z Z
else instance afterTagsFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "s" "" Z (S n)
else instance afterTagsFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "s" "" (S n) x
else instance afterTags :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "s" rest sq ltgt
else instance afterTagtClean :: AfterTag "t" "" Z Z
else instance afterTagtFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "t" "" Z (S n)
else instance afterTagtFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "t" "" (S n) x
else instance afterTagt :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "t" rest sq ltgt
else instance afterTaguClean :: AfterTag "u" "" Z Z
else instance afterTaguFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "u" "" Z (S n)
else instance afterTaguFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "u" "" (S n) x
else instance afterTagu :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "u" rest sq ltgt
else instance afterTagvClean :: AfterTag "v" "" Z Z
else instance afterTagvFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "v" "" Z (S n)
else instance afterTagvFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "v" "" (S n) x
else instance afterTagv :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "v" rest sq ltgt
else instance afterTagwClean :: AfterTag "w" "" Z Z
else instance afterTagwFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "w" "" Z (S n)
else instance afterTagwFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "w" "" (S n) x
else instance afterTagw :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "w" rest sq ltgt
else instance afterTagxClean :: AfterTag "x" "" Z Z
else instance afterTagxFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "x" "" Z (S n)
else instance afterTagxFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "x" "" (S n) x
else instance afterTagx :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "x" rest sq ltgt
else instance afterTagyClean :: AfterTag "y" "" Z Z
else instance afterTagyFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "y" "" Z (S n)
else instance afterTagyFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "y" "" (S n) x
else instance afterTagy :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "y" rest sq ltgt
else instance afterTagzClean :: AfterTag "z" "" Z Z
else instance afterTagzFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "z" "" Z (S n)
else instance afterTagzFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "z" "" (S n) x
else instance afterTagz :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "z" rest sq ltgt
else instance afterTagAClean :: AfterTag "A" "" Z Z
else instance afterTagAFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "A" "" Z (S n)
else instance afterTagAFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "A" "" (S n) x
else instance afterTagA :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "A" rest sq ltgt
else instance afterTagBClean :: AfterTag "B" "" Z Z
else instance afterTagBFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "B" "" Z (S n)
else instance afterTagBFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "B" "" (S n) x
else instance afterTagB :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "B" rest sq ltgt
else instance afterTagCClean :: AfterTag "C" "" Z Z
else instance afterTagCFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "C" "" Z (S n)
else instance afterTagCFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "C" "" (S n) x
else instance afterTagC :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "C" rest sq ltgt
else instance afterTagDClean :: AfterTag "D" "" Z Z
else instance afterTagDFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "D" "" Z (S n)
else instance afterTagDFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "D" "" (S n) x
else instance afterTagD :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "D" rest sq ltgt
else instance afterTagEClean :: AfterTag "E" "" Z Z
else instance afterTagEFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "E" "" Z (S n)
else instance afterTagEFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "E" "" (S n) x
else instance afterTagE :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "E" rest sq ltgt
else instance afterTagFClean :: AfterTag "F" "" Z Z
else instance afterTagFFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "F" "" Z (S n)
else instance afterTagFFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "F" "" (S n) x
else instance afterTagF :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "F" rest sq ltgt
else instance afterTagGClean :: AfterTag "G" "" Z Z
else instance afterTagGFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "G" "" Z (S n)
else instance afterTagGFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "G" "" (S n) x
else instance afterTagG :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "G" rest sq ltgt
else instance afterTagHClean :: AfterTag "H" "" Z Z
else instance afterTagHFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "H" "" Z (S n)
else instance afterTagHFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "H" "" (S n) x
else instance afterTagH :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "H" rest sq ltgt
else instance afterTagIClean :: AfterTag "I" "" Z Z
else instance afterTagIFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "I" "" Z (S n)
else instance afterTagIFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "I" "" (S n) x
else instance afterTagI :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "I" rest sq ltgt
else instance afterTagJClean :: AfterTag "J" "" Z Z
else instance afterTagJFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "J" "" Z (S n)
else instance afterTagJFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "J" "" (S n) x
else instance afterTagJ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "J" rest sq ltgt
else instance afterTagKClean :: AfterTag "K" "" Z Z
else instance afterTagKFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "K" "" Z (S n)
else instance afterTagKFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "K" "" (S n) x
else instance afterTagK :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "K" rest sq ltgt
else instance afterTagLClean :: AfterTag "L" "" Z Z
else instance afterTagLFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "L" "" Z (S n)
else instance afterTagLFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "L" "" (S n) x
else instance afterTagL :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "L" rest sq ltgt
else instance afterTagMClean :: AfterTag "M" "" Z Z
else instance afterTagMFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "M" "" Z (S n)
else instance afterTagMFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "M" "" (S n) x
else instance afterTagM :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "M" rest sq ltgt
else instance afterTagNClean :: AfterTag "N" "" Z Z
else instance afterTagNFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "N" "" Z (S n)
else instance afterTagNFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "N" "" (S n) x
else instance afterTagN :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "N" rest sq ltgt
else instance afterTagOClean :: AfterTag "O" "" Z Z
else instance afterTagOFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "O" "" Z (S n)
else instance afterTagOFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "O" "" (S n) x
else instance afterTagO :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "O" rest sq ltgt
else instance afterTagPClean :: AfterTag "P" "" Z Z
else instance afterTagPFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "P" "" Z (S n)
else instance afterTagPFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "P" "" (S n) x
else instance afterTagP :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "P" rest sq ltgt
else instance afterTagQClean :: AfterTag "Q" "" Z Z
else instance afterTagQFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "Q" "" Z (S n)
else instance afterTagQFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "Q" "" (S n) x
else instance afterTagQ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "Q" rest sq ltgt
else instance afterTagRClean :: AfterTag "R" "" Z Z
else instance afterTagRFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "R" "" Z (S n)
else instance afterTagRFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "R" "" (S n) x
else instance afterTagR :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "R" rest sq ltgt
else instance afterTagSClean :: AfterTag "S" "" Z Z
else instance afterTagSFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "S" "" Z (S n)
else instance afterTagSFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "S" "" (S n) x
else instance afterTagS :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "S" rest sq ltgt
else instance afterTagTClean :: AfterTag "T" "" Z Z
else instance afterTagTFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "T" "" Z (S n)
else instance afterTagTFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "T" "" (S n) x
else instance afterTagT :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "T" rest sq ltgt
else instance afterTagUClean :: AfterTag "U" "" Z Z
else instance afterTagUFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "U" "" Z (S n)
else instance afterTagUFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "U" "" (S n) x
else instance afterTagU :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "U" rest sq ltgt
else instance afterTagVClean :: AfterTag "V" "" Z Z
else instance afterTagVFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "V" "" Z (S n)
else instance afterTagVFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "V" "" (S n) x
else instance afterTagV :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "V" rest sq ltgt
else instance afterTagWClean :: AfterTag "W" "" Z Z
else instance afterTagWFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "W" "" Z (S n)
else instance afterTagWFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "W" "" (S n) x
else instance afterTagW :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "W" rest sq ltgt
else instance afterTagXClean :: AfterTag "X" "" Z Z
else instance afterTagXFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "X" "" Z (S n)
else instance afterTagXFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "X" "" (S n) x
else instance afterTagX :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "X" rest sq ltgt
else instance afterTagYClean :: AfterTag "Y" "" Z Z
else instance afterTagYFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "Y" "" Z (S n)
else instance afterTagYFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "Y" "" (S n) x
else instance afterTagY :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "Y" rest sq ltgt
else instance afterTagZClean :: AfterTag "Z" "" Z Z
else instance afterTagZFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "Z" "" Z (S n)
else instance afterTagZFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "Z" "" (S n) x
else instance afterTagZ :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "Z" rest sq ltgt
else instance afterTag0Clean :: AfterTag "0" "" Z Z
else instance afterTag0FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "0" "" Z (S n)
else instance afterTag0FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "0" "" (S n) x
else instance afterTag0 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "0" rest sq ltgt
else instance afterTag1Clean :: AfterTag "1" "" Z Z
else instance afterTag1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "1" "" Z (S n)
else instance afterTag1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "1" "" (S n) x
else instance afterTag1 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "1" rest sq ltgt
else instance afterTag2Clean :: AfterTag "2" "" Z Z
else instance afterTag2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "2" "" Z (S n)
else instance afterTag2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "2" "" (S n) x
else instance afterTag2 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "2" rest sq ltgt
else instance afterTag3Clean :: AfterTag "3" "" Z Z
else instance afterTag3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "3" "" Z (S n)
else instance afterTag3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "3" "" (S n) x
else instance afterTag3 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "3" rest sq ltgt
else instance afterTag4Clean :: AfterTag "4" "" Z Z
else instance afterTag4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "4" "" Z (S n)
else instance afterTag4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "4" "" (S n) x
else instance afterTag4 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "4" rest sq ltgt
else instance afterTag5Clean :: AfterTag "5" "" Z Z
else instance afterTag5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "5" "" Z (S n)
else instance afterTag5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "5" "" (S n) x
else instance afterTag5 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "5" rest sq ltgt
else instance afterTag6Clean :: AfterTag "6" "" Z Z
else instance afterTag6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "6" "" Z (S n)
else instance afterTag6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "6" "" (S n) x
else instance afterTag6 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "6" rest sq ltgt
else instance afterTag7Clean :: AfterTag "7" "" Z Z
else instance afterTag7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "7" "" Z (S n)
else instance afterTag7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "7" "" (S n) x
else instance afterTag7 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "7" rest sq ltgt
else instance afterTag8Clean :: AfterTag "8" "" Z Z
else instance afterTag8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "8" "" Z (S n)
else instance afterTag8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "8" "" (S n) x
else instance afterTag8 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "8" rest sq ltgt
else instance afterTag9Clean :: AfterTag "9" "" Z Z
else instance afterTag9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "9" "" Z (S n)
else instance afterTag9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "9" "" (S n) x
else instance afterTag9 :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "9" rest sq ltgt
else instance afterTagColonClean :: AfterTag ":" "" Z Z
else instance afterTagColonFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag ":" "" Z (S n)
else instance afterTagColonFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag ":" "" (S n) x
else instance afterTagColon :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag ":" rest sq ltgt
else instance afterTagTildeClean :: AfterTag "~" "" Z Z
else instance afterTagTildeFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "~" "" Z (S n)
else instance afterTagTildeFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "~" "" (S n) x
else instance afterTagTilde :: (Cons head tail rest, AfterNominal head tail sq ltgt) => AfterTag "~" rest sq ltgt
else instance afterTagSpaceClean :: AfterTag " " "" Z Z
else instance afterTagSpaceFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag " " "" Z (S n)
else instance afterTagSpaceFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag " " "" (S n) x
else instance afterTagSpace :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag " " rest sq ltgt
else instance afterTagTabClean :: AfterTag "\t" "" Z Z
else instance afterTagTabFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "\t" "" Z (S n)
else instance afterTagTabFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "\t" "" (S n) x
else instance afterTagTab :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag "\t" rest sq ltgt
else instance afterTagNewLineClean :: AfterTag "\n" "" Z Z
else instance afterTagNewLineFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "\n" "" Z (S n)
else instance afterTagNewLineFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "\n" "" (S n) x
else instance afterTagNewLine :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag "\n" rest sq ltgt
else instance afterTagCommaClean :: AfterTag "," "" Z Z
else instance afterTagCommaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "," "" Z (S n)
else instance afterTagCommaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "," "" (S n) x
else instance afterTagComma :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag "," rest sq ltgt
else instance afterTagOpenSquareClean :: AfterTag "[" "" Z Z
else instance afterTagOpenSquareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "[" "" Z (S n)
else instance afterTagOpenSquareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "[" "" (S n) x
else instance afterTagOpenSquare :: (Cons head tail rest, AfterSpecialChar head tail (S sq) ltgt) => AfterTag "[" rest sq ltgt
else instance afterTagLessThanClean :: AfterTag "<" "" Z Z
else instance afterTagLessThanFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterTag "<" "" Z (S n)
else instance afterTagLessThanFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterTag "<" "" (S n) x
else instance afterTagLessThan :: (Cons head tail rest, AfterSpecialChar head tail sq (S ltgt)) => AfterTag "<" rest sq ltgt
else instance afterTagValidGt :: AfterTag ">" "" Z (S Z)
else instance afterTagValidSq :: AfterTag "]" "" (S Z) Z
else instance afterTagTooManyGt :: Fail (Text "Too many closing >") => AfterTag ">" "" Z Z
else instance afterTagTooManySq :: Fail (Text "Too many closing ]") => AfterTag "]" "" Z Z
else instance afterTagNotEnoughGt :: Fail (Text "Not enough closing >") => AfterTag ">" "" Z (S ltgt)
else instance afterTagNotEnoughSq :: Fail (Text "Not enough closing ]") => AfterTag "]" "" (S sq) Z
else instance afterTagContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag ">" rest sq (S ltgt)
else instance afterTagContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterTag "]" rest (S sq) ltgt
else instance afterTagUnderfullGt :: Fail (Text "Too many closing >") => AfterTag ">" rest sq Z
else instance afterTagUnderfullSq :: Fail (Text "Too many closing ]") => AfterTag "]" rest Z ltgt
else instance afterTagSpurious :: Fail (Text "Not a valid symbol") => AfterTag invalid rest Z Z
-- starting bloc AfterMult
class AfterMult (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterMult1Clean :: AfterMult "1" "" Z Z
else instance afterMult1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "1" "" Z (S n)
else instance afterMult1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "1" "" (S n) x
else instance afterMult1 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "1" rest sq ltgt
else instance afterMult2Clean :: AfterMult "2" "" Z Z
else instance afterMult2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "2" "" Z (S n)
else instance afterMult2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "2" "" (S n) x
else instance afterMult2 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "2" rest sq ltgt
else instance afterMult3Clean :: AfterMult "3" "" Z Z
else instance afterMult3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "3" "" Z (S n)
else instance afterMult3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "3" "" (S n) x
else instance afterMult3 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "3" rest sq ltgt
else instance afterMult4Clean :: AfterMult "4" "" Z Z
else instance afterMult4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "4" "" Z (S n)
else instance afterMult4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "4" "" (S n) x
else instance afterMult4 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "4" rest sq ltgt
else instance afterMult5Clean :: AfterMult "5" "" Z Z
else instance afterMult5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "5" "" Z (S n)
else instance afterMult5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "5" "" (S n) x
else instance afterMult5 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "5" rest sq ltgt
else instance afterMult6Clean :: AfterMult "6" "" Z Z
else instance afterMult6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "6" "" Z (S n)
else instance afterMult6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "6" "" (S n) x
else instance afterMult6 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "6" rest sq ltgt
else instance afterMult7Clean :: AfterMult "7" "" Z Z
else instance afterMult7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "7" "" Z (S n)
else instance afterMult7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "7" "" (S n) x
else instance afterMult7 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "7" rest sq ltgt
else instance afterMult8Clean :: AfterMult "8" "" Z Z
else instance afterMult8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "8" "" Z (S n)
else instance afterMult8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "8" "" (S n) x
else instance afterMult8 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "8" rest sq ltgt
else instance afterMult9Clean :: AfterMult "9" "" Z Z
else instance afterMult9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterMult "9" "" Z (S n)
else instance afterMult9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterMult "9" "" (S n) x
else instance afterMult9 :: (Cons head tail rest, AfterFirstDigit head tail sq ltgt) => AfterMult "9" rest sq ltgt
else instance afterMultSpurious :: Fail (Text "Not a valid symbol") => AfterMult invalid rest Z Z
-- starting bloc AfterFirstDigit
class AfterFirstDigit (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterFirstDigit0Clean :: AfterFirstDigit "0" "" Z Z
else instance afterFirstDigit0FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "0" "" Z (S n)
else instance afterFirstDigit0FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "0" "" (S n) x
else instance afterFirstDigit0 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "0" rest sq ltgt
else instance afterFirstDigit1Clean :: AfterFirstDigit "1" "" Z Z
else instance afterFirstDigit1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "1" "" Z (S n)
else instance afterFirstDigit1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "1" "" (S n) x
else instance afterFirstDigit1 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "1" rest sq ltgt
else instance afterFirstDigit2Clean :: AfterFirstDigit "2" "" Z Z
else instance afterFirstDigit2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "2" "" Z (S n)
else instance afterFirstDigit2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "2" "" (S n) x
else instance afterFirstDigit2 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "2" rest sq ltgt
else instance afterFirstDigit3Clean :: AfterFirstDigit "3" "" Z Z
else instance afterFirstDigit3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "3" "" Z (S n)
else instance afterFirstDigit3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "3" "" (S n) x
else instance afterFirstDigit3 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "3" rest sq ltgt
else instance afterFirstDigit4Clean :: AfterFirstDigit "4" "" Z Z
else instance afterFirstDigit4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "4" "" Z (S n)
else instance afterFirstDigit4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "4" "" (S n) x
else instance afterFirstDigit4 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "4" rest sq ltgt
else instance afterFirstDigit5Clean :: AfterFirstDigit "5" "" Z Z
else instance afterFirstDigit5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "5" "" Z (S n)
else instance afterFirstDigit5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "5" "" (S n) x
else instance afterFirstDigit5 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "5" rest sq ltgt
else instance afterFirstDigit6Clean :: AfterFirstDigit "6" "" Z Z
else instance afterFirstDigit6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "6" "" Z (S n)
else instance afterFirstDigit6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "6" "" (S n) x
else instance afterFirstDigit6 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "6" rest sq ltgt
else instance afterFirstDigit7Clean :: AfterFirstDigit "7" "" Z Z
else instance afterFirstDigit7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "7" "" Z (S n)
else instance afterFirstDigit7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "7" "" (S n) x
else instance afterFirstDigit7 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "7" rest sq ltgt
else instance afterFirstDigit8Clean :: AfterFirstDigit "8" "" Z Z
else instance afterFirstDigit8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "8" "" Z (S n)
else instance afterFirstDigit8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "8" "" (S n) x
else instance afterFirstDigit8 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "8" rest sq ltgt
else instance afterFirstDigit9Clean :: AfterFirstDigit "9" "" Z Z
else instance afterFirstDigit9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "9" "" Z (S n)
else instance afterFirstDigit9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "9" "" (S n) x
else instance afterFirstDigit9 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterFirstDigit "9" rest sq ltgt
else instance afterFirstDigitSpaceClean :: AfterFirstDigit " " "" Z Z
else instance afterFirstDigitSpaceFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit " " "" Z (S n)
else instance afterFirstDigitSpaceFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit " " "" (S n) x
else instance afterFirstDigitSpace :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit " " rest sq ltgt
else instance afterFirstDigitTabClean :: AfterFirstDigit "\t" "" Z Z
else instance afterFirstDigitTabFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "\t" "" Z (S n)
else instance afterFirstDigitTabFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "\t" "" (S n) x
else instance afterFirstDigitTab :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit "\t" rest sq ltgt
else instance afterFirstDigitNewLineClean :: AfterFirstDigit "\n" "" Z Z
else instance afterFirstDigitNewLineFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "\n" "" Z (S n)
else instance afterFirstDigitNewLineFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "\n" "" (S n) x
else instance afterFirstDigitNewLine :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit "\n" rest sq ltgt
else instance afterFirstDigitCommaClean :: AfterFirstDigit "," "" Z Z
else instance afterFirstDigitCommaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "," "" Z (S n)
else instance afterFirstDigitCommaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "," "" (S n) x
else instance afterFirstDigitComma :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit "," rest sq ltgt
else instance afterFirstDigitOpenSquareClean :: AfterFirstDigit "[" "" Z Z
else instance afterFirstDigitOpenSquareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "[" "" Z (S n)
else instance afterFirstDigitOpenSquareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "[" "" (S n) x
else instance afterFirstDigitOpenSquare :: (Cons head tail rest, AfterSpecialChar head tail (S sq) ltgt) => AfterFirstDigit "[" rest sq ltgt
else instance afterFirstDigitLessThanClean :: AfterFirstDigit "<" "" Z Z
else instance afterFirstDigitLessThanFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterFirstDigit "<" "" Z (S n)
else instance afterFirstDigitLessThanFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterFirstDigit "<" "" (S n) x
else instance afterFirstDigitLessThan :: (Cons head tail rest, AfterSpecialChar head tail sq (S ltgt)) => AfterFirstDigit "<" rest sq ltgt
else instance afterFirstDigitValidGt :: AfterFirstDigit ">" "" Z (S Z)
else instance afterFirstDigitValidSq :: AfterFirstDigit "]" "" (S Z) Z
else instance afterFirstDigitTooManyGt :: Fail (Text "Too many closing >") => AfterFirstDigit ">" "" Z Z
else instance afterFirstDigitTooManySq :: Fail (Text "Too many closing ]") => AfterFirstDigit "]" "" Z Z
else instance afterFirstDigitNotEnoughGt :: Fail (Text "Not enough closing >") => AfterFirstDigit ">" "" Z (S ltgt)
else instance afterFirstDigitNotEnoughSq :: Fail (Text "Not enough closing ]") => AfterFirstDigit "]" "" (S sq) Z
else instance afterFirstDigitContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit ">" rest sq (S ltgt)
else instance afterFirstDigitContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterFirstDigit "]" rest (S sq) ltgt
else instance afterFirstDigitUnderfullGt :: Fail (Text "Too many closing >") => AfterFirstDigit ">" rest sq Z
else instance afterFirstDigitUnderfullSq :: Fail (Text "Too many closing ]") => AfterFirstDigit "]" rest Z ltgt
else instance afterFirstDigitSpurious :: Fail (Text "Not a valid symbol") => AfterFirstDigit invalid rest Z Z
-- starting bloc AfterSubsequentDigit
class AfterSubsequentDigit (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)
instance afterSubsequentDigit0Clean :: AfterSubsequentDigit "0" "" Z Z
else instance afterSubsequentDigit0FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "0" "" Z (S n)
else instance afterSubsequentDigit0FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "0" "" (S n) x
else instance afterSubsequentDigit0 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "0" rest sq ltgt
else instance afterSubsequentDigit1Clean :: AfterSubsequentDigit "1" "" Z Z
else instance afterSubsequentDigit1FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "1" "" Z (S n)
else instance afterSubsequentDigit1FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "1" "" (S n) x
else instance afterSubsequentDigit1 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "1" rest sq ltgt
else instance afterSubsequentDigit2Clean :: AfterSubsequentDigit "2" "" Z Z
else instance afterSubsequentDigit2FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "2" "" Z (S n)
else instance afterSubsequentDigit2FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "2" "" (S n) x
else instance afterSubsequentDigit2 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "2" rest sq ltgt
else instance afterSubsequentDigit3Clean :: AfterSubsequentDigit "3" "" Z Z
else instance afterSubsequentDigit3FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "3" "" Z (S n)
else instance afterSubsequentDigit3FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "3" "" (S n) x
else instance afterSubsequentDigit3 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "3" rest sq ltgt
else instance afterSubsequentDigit4Clean :: AfterSubsequentDigit "4" "" Z Z
else instance afterSubsequentDigit4FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "4" "" Z (S n)
else instance afterSubsequentDigit4FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "4" "" (S n) x
else instance afterSubsequentDigit4 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "4" rest sq ltgt
else instance afterSubsequentDigit5Clean :: AfterSubsequentDigit "5" "" Z Z
else instance afterSubsequentDigit5FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "5" "" Z (S n)
else instance afterSubsequentDigit5FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "5" "" (S n) x
else instance afterSubsequentDigit5 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "5" rest sq ltgt
else instance afterSubsequentDigit6Clean :: AfterSubsequentDigit "6" "" Z Z
else instance afterSubsequentDigit6FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "6" "" Z (S n)
else instance afterSubsequentDigit6FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "6" "" (S n) x
else instance afterSubsequentDigit6 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "6" rest sq ltgt
else instance afterSubsequentDigit7Clean :: AfterSubsequentDigit "7" "" Z Z
else instance afterSubsequentDigit7FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "7" "" Z (S n)
else instance afterSubsequentDigit7FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "7" "" (S n) x
else instance afterSubsequentDigit7 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "7" rest sq ltgt
else instance afterSubsequentDigit8Clean :: AfterSubsequentDigit "8" "" Z Z
else instance afterSubsequentDigit8FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "8" "" Z (S n)
else instance afterSubsequentDigit8FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "8" "" (S n) x
else instance afterSubsequentDigit8 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "8" rest sq ltgt
else instance afterSubsequentDigit9Clean :: AfterSubsequentDigit "9" "" Z Z
else instance afterSubsequentDigit9FailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "9" "" Z (S n)
else instance afterSubsequentDigit9FailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "9" "" (S n) x
else instance afterSubsequentDigit9 :: (Cons head tail rest, AfterSubsequentDigit head tail sq ltgt) => AfterSubsequentDigit "9" rest sq ltgt
else instance afterSubsequentDigitSpaceClean :: AfterSubsequentDigit " " "" Z Z
else instance afterSubsequentDigitSpaceFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit " " "" Z (S n)
else instance afterSubsequentDigitSpaceFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit " " "" (S n) x
else instance afterSubsequentDigitSpace :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit " " rest sq ltgt
else instance afterSubsequentDigitTabClean :: AfterSubsequentDigit "\t" "" Z Z
else instance afterSubsequentDigitTabFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "\t" "" Z (S n)
else instance afterSubsequentDigitTabFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "\t" "" (S n) x
else instance afterSubsequentDigitTab :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit "\t" rest sq ltgt
else instance afterSubsequentDigitNewLineClean :: AfterSubsequentDigit "\n" "" Z Z
else instance afterSubsequentDigitNewLineFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "\n" "" Z (S n)
else instance afterSubsequentDigitNewLineFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "\n" "" (S n) x
else instance afterSubsequentDigitNewLine :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit "\n" rest sq ltgt
else instance afterSubsequentDigitCommaClean :: AfterSubsequentDigit "," "" Z Z
else instance afterSubsequentDigitCommaFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "," "" Z (S n)
else instance afterSubsequentDigitCommaFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "," "" (S n) x
else instance afterSubsequentDigitComma :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit "," rest sq ltgt
else instance afterSubsequentDigitOpenSquareClean :: AfterSubsequentDigit "[" "" Z Z
else instance afterSubsequentDigitOpenSquareFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "[" "" Z (S n)
else instance afterSubsequentDigitOpenSquareFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "[" "" (S n) x
else instance afterSubsequentDigitOpenSquare :: (Cons head tail rest, AfterSpecialChar head tail (S sq) ltgt) => AfterSubsequentDigit "[" rest sq ltgt
else instance afterSubsequentDigitLessThanClean :: AfterSubsequentDigit "<" "" Z Z
else instance afterSubsequentDigitLessThanFailLtGt :: Fail (Text "Parser found a < without a closing >") => AfterSubsequentDigit "<" "" Z (S n)
else instance afterSubsequentDigitLessThanFailSq :: Fail (Text "Parser found a [ without a closing ]") => AfterSubsequentDigit "<" "" (S n) x
else instance afterSubsequentDigitLessThan :: (Cons head tail rest, AfterSpecialChar head tail sq (S ltgt)) => AfterSubsequentDigit "<" rest sq ltgt
else instance afterSubsequentDigitValidGt :: AfterSubsequentDigit ">" "" Z (S Z)
else instance afterSubsequentDigitValidSq :: AfterSubsequentDigit "]" "" (S Z) Z
else instance afterSubsequentDigitTooManyGt :: Fail (Text "Too many closing >") => AfterSubsequentDigit ">" "" Z Z
else instance afterSubsequentDigitTooManySq :: Fail (Text "Too many closing ]") => AfterSubsequentDigit "]" "" Z Z
else instance afterSubsequentDigitNotEnoughGt :: Fail (Text "Not enough closing >") => AfterSubsequentDigit ">" "" Z (S ltgt)
else instance afterSubsequentDigitNotEnoughSq :: Fail (Text "Not enough closing ]") => AfterSubsequentDigit "]" "" (S sq) Z
else instance afterSubsequentDigitContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit ">" rest sq (S ltgt)
else instance afterSubsequentDigitContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => AfterSubsequentDigit "]" rest (S sq) ltgt
else instance afterSubsequentDigitUnderfullGt :: Fail (Text "Too many closing >") => AfterSubsequentDigit ">" rest sq Z
else instance afterSubsequentDigitUnderfullSq :: Fail (Text "Too many closing ]") => AfterSubsequentDigit "]" rest Z ltgt
else instance afterSubsequentDigitSpurious :: Fail (Text "Not a valid symbol") => AfterSubsequentDigit invalid rest Z Z

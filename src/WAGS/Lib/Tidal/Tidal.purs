module WAGS.Lib.Tidal.Tidal
  ( module WAGS.Lib.Tidal.Cycle
  , make
  , parse
  , parse_
  , impatient
  , plainly
  , s
  , rend
  , rend_
  , rendNit
  , c2s
  , s2f
  , openFuture
  , massiveFuture
  , droneyFuture
  , reFuture
  ---
  , djQuickCheck
  ---
  , b
  , i
  , x
  , b'
  , i'
  , x'
  , b_
  , i_
  , x_
  , u
  ---
  , onTagsCWithIndex
  , onTagsCWithIndex'
  , onTagsC
  , onTagsC'
  , onTagsWithIndex
  , onTagsWithIndex'
  , onTags
  , onTags'
  , onTagWithIndex
  , onTagWithIndex'
  , onTag
  , onTag'
  ---
  , lvg
  , lvt
  , lfn
  , lft
  , lfb
  , lfl
  , lfc
  , lfd
  , ltn
  , lts
  , ltd
  , ltt
  , lns
  , lnr
  , lnbo
  , lnf
  , lnv
  , lds
  , ldr
  , ldls
  , ldle
  , ldf
  , ldv
  , lcw
  , lct
  , ldt
  ---
  , when_
  , focus
  , mapmap
  , betwixt
  , derivative
  ---
  , asScore
  , openVoice
  , cycleP
  , cycleP_
  , unrest
  , class S
  , intentionalSilenceForInternalUseOnly
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Control.Monad.State (evalState, get, put)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), hush)
import Data.Filterable (compact, filter, filterMap, maybeBool)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString, toNumber)
import Data.Lens (Lens', Prism', _Just, lens, over, prism', set)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List (List(..), foldMap, foldl, (:))
import Data.List as L
import Data.List.NonEmpty (sortBy)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Traversable (class Foldable, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D1)
import Data.Unfoldable (replicate)
import Data.Vec ((+>))
import Data.Vec as V
import Foreign.Object as O
import Heterogeneous.Mapping (hmap, hmapWithIndex)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf1, elements, frequency, resize)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodeUnits (satisfy, oneOf, skipSpaces, string, alphaNum, anyDigit, char)
import Text.Parsing.StringParser.Combinators (between, many, many1, optionMaybe, sepBy, sepEndBy)
import Type.Proxy (Proxy(..))
import WAGS.Create (class Create)
import WAGS.Create.Optionals (input)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Tumult (Tumultuous)
import WAGS.Tumult.Make (tumultuously)
import WAGS.Validation (class NodesCanBeTumultuous, class SubgraphIsRenderable)
import WAGS.Lib.Tidal.Cycle (Cycle(..), flattenCycle, intentionalSilenceForInternalUseOnly_, reverse)
import WAGS.Lib.Tidal.FX (WAGSITumult)
import WAGS.Lib.Tidal.SampleDurs (sampleToDur, sampleToDur')
import WAGS.Lib.Tidal.Samples (class ClockTime, clockTime, dronesToSample, nameToSample)
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.Types (AH, AH', AfterMatter, BufferUrl, ClockTimeIs, CycleDuration(..), DroneNote(..), EWF, EWF', FoT, Globals(..), ICycle(..), NextCycle(..), Note(..), NoteInFlattenedTime(..), NoteInTime(..), O'Past, Sample(..), Tag, TheFuture(..), TimeIsAndWas, UnsampledTimeIs, Voice(..), ZipProps(..))

-- | Only play the first cycle, and truncate/interrupt the playing cycle at the next sub-ending.
impatient :: NextCycle ~> NextCycle
impatient = over (unto NextCycle <<< prop (Proxy :: _ "force")) (const true)

make
  :: forall inRec overfull rest event
   . Union inRec
       ( EWF' (CycleDuration -> Voice event)
           ( AH' (Maybe (DroneNote event))
               (title :: String, sounds :: Map.Map Sample BufferUrl, preload :: Array Sample)
           )
       )
       overfull
  => Nub overfull
       ( EWF' (CycleDuration -> Voice event)
           ( AH' (Maybe (DroneNote event))
               (title :: String, sounds :: Map.Map Sample BufferUrl, preload :: Array Sample | rest)
           )
       )
  => Number
  -> { | inRec }
  -> TheFuture event
make cl rr = TheFuture $ Record.union
  ( hmapWithIndex (ZipProps z)
      ( hmap (\(_ :: (CycleDuration -> Voice event)) -> (wrap cl))
          { earth: z.earth
          , wind: z.wind
          , fire: z.fire
          }
      )
  )
  { air: z.air, heart: z.heart, title: z.title, sounds: z.sounds, preload: z.preload }
  where
  z =
    Record.merge rr
      ( Record.union (openVoices :: OpenVoices event)
          $ Record.union (openDrones :: OpenDrones event)
              { title: "wagsi @ tidal"
              , preload: [] :: Array Sample
              , sounds: Map.empty :: Map.Map Sample BufferUrl
              }
      )
      ::
           { | EWF' (CycleDuration -> Voice event)
               ( AH' (Maybe (DroneNote event))
                   ( title :: String
                   , sounds :: Map.Map Sample BufferUrl
                   , preload :: Array Sample
                   | rest
                   )
               )
           }

fx
  :: forall scene graph graphRL
   . RowToList graph graphRL
  => Create scene () graph
  => SubgraphIsRenderable graph "output" (voice :: Unit)
  => NodesCanBeTumultuous graphRL
  => { | scene }
  -> Tumultuous D1 "output" (voice :: Unit)
fx scene = tumultuously (scene +> V.empty)

hello :: CTOR.Input "voice" /\ {}
hello = input (Proxy :: _ "voice")

goodbye :: forall a. a -> { output :: a }
goodbye = { output: _ }

calm :: Tumultuous D1 "output" (voice :: Unit)
calm = fx $ goodbye hello

plainly :: NextCycle ~> Voice
plainly = Voice <<< { globals: Globals { gain: const 1.0, fx: const calm }, next: _ }

--- lenses
lvg :: forall event. Lens' (Voice event) (O'Past event)
lvg = unto Voice <<< prop (Proxy :: _ "globals") <<< unto Globals <<< prop (Proxy :: _ "gain")

lvt :: forall event. Lens' (Voice event) (ClockTimeIs event -> Tumultuous D1 "output" (voice :: Unit))
lvt = unto Voice <<< prop (Proxy :: _ "globals") <<< unto Globals <<< prop (Proxy :: _ "fx")

lfn :: forall note. Lens' (NoteInFlattenedTime note) note
lfn = unto NoteInFlattenedTime <<< prop (Proxy :: _ "note")

lfb :: forall note. Lens' (NoteInFlattenedTime note) Number
lfb = unto NoteInFlattenedTime <<< prop (Proxy :: _ "bigStartsAt")

lfl :: forall note. Lens' (NoteInFlattenedTime note) Number
lfl = unto NoteInFlattenedTime <<< prop (Proxy :: _ "littleStartsAt")

lfc :: forall note. Lens' (NoteInFlattenedTime note) Int
lfc = unto NoteInFlattenedTime <<< prop (Proxy :: _ "currentCycle")

lfd :: forall note. Lens' (NoteInFlattenedTime note) Number
lfd = unto NoteInFlattenedTime <<< prop (Proxy :: _ "duration")

lft :: forall p note. Choice p => Strong p => p String String -> p (NoteInFlattenedTime note) (NoteInFlattenedTime note)
lft = unto NoteInFlattenedTime <<< prop (Proxy :: _ "tag") <<< _Just

ltn :: forall note. Lens' (NoteInTime note) note
ltn = unto NoteInTime <<< prop (Proxy :: _ "note")

lts :: forall note. Lens' (NoteInTime note) Number
lts = unto NoteInTime <<< prop (Proxy :: _ "startsAt")

ltd :: forall note. Lens' (NoteInTime note) Number
ltd = unto NoteInTime <<< prop (Proxy :: _ "duration")

ltt :: forall p note. Choice p => Strong p => p String String -> p (NoteInTime note) (NoteInTime note)
ltt = unto NoteInTime <<< prop (Proxy :: _ "tag") <<< _Just

lns :: forall event. Lens' (Note event) (Either (UnsampledTimeIs event -> Sample) Sample)
lns = unto Note <<< prop (Proxy :: _ "sampleFoT")

lnr :: forall event. Lens' (Note event) (FoT event)
lnr = unto Note <<< prop (Proxy :: _ "rateFoT")

lnbo :: forall event. Lens' (Note event) (FoT event)
lnbo = unto Note <<< prop (Proxy :: _ "bufferOffsetFoT")

lnf :: forall event. Lens' (Note event) Boolean
lnf = unto Note <<< prop (Proxy :: _ "forward")

lnv :: forall event. Lens' (Note event) (FoT event)
lnv = unto Note <<< prop (Proxy :: _ "volumeFoT")

lds :: forall event. Lens' (DroneNote event) Sample
lds = unto DroneNote <<< prop (Proxy :: _ "sample")

ldr :: forall event. Lens' (DroneNote event) (O'Past event)
ldr = unto DroneNote <<< prop (Proxy :: _ "rateFoT")

ldls :: forall event. Lens' (DroneNote event) (O'Past event)
ldls = unto DroneNote <<< prop (Proxy :: _ "loopStartFoT")

ldle :: forall event. Lens' (DroneNote event) (O'Past event)
ldle = unto DroneNote <<< prop (Proxy :: _ "loopEndFoT")

ldf :: forall event. Lens' (DroneNote event) Boolean
ldf = unto DroneNote <<< prop (Proxy :: _ "forward")

ldv :: forall event. Lens' (DroneNote event) (O'Past event)
ldv = unto DroneNote <<< prop (Proxy :: _ "volumeFoT")

ldt :: forall event. Lens' (DroneNote event) (ClockTimeIs event -> WAGSITumult)
ldt = unto DroneNote <<< prop (Proxy :: _ "tumultFoT")

lcw :: forall note. Lens' (Cycle note) Number
lcw = lens getWeight
  ( case _ of
      Branching ii -> \weight -> Branching $ ii { env = ii.env { weight = weight } }
      Simultaneous ii -> \weight -> Simultaneous $ ii { env = ii.env { weight = weight } }
      Internal ii -> \weight -> Internal $ ii { env = ii.env { weight = weight } }
      SingleNote ii -> \weight -> SingleNote $ ii { env = ii.env { weight = weight } }
  )

lct :: forall note. Lens' (Cycle note) (Maybe String)
lct = lens getTag
  ( case _ of
      Branching ii -> \tag -> Branching $ ii { env = ii.env { tag = tag } }
      Simultaneous ii -> \tag -> Simultaneous $ ii { env = ii.env { tag = tag } }
      Internal ii -> \tag -> Internal $ ii { env = ii.env { tag = tag } }
      SingleNote ii -> \tag -> SingleNote $ ii { env = ii.env { tag = tag } }
  )

---

when_ :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
when_ cond func aa = if cond aa then func aa else aa

focus :: forall a. (a -> Boolean) -> Prism' a a
focus = prism' identity <<< maybeBool

---

mapmap :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
mapmap = map <<< map

---
b :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
b bx by = Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (bx :| L.fromFoldable by) }

b' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
b' bx = b bx []

b_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
b_ = b

i :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
i sx sy = Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (sx :| L.fromFoldable sy) }

i' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
i' sx = i sx []

i_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
i_ = i

x :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
x xx xy = Simultaneous { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (xx :| L.fromFoldable xy) }

x' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
x' sx = x sx []

x_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
x_ = x

u :: Cycle (Maybe (Note Unit)) -> Cycle (Maybe (Note Unit))
u = identity

sampleName :: Parser String
sampleName = map (fromCharArray <<< A.fromFoldable <<< NEL.toList) (many1 $ oneOf [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', '~' ])

---
whiteSpace1 :: Parser String
whiteSpace1 = do
  cs <- many1 (satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap singleton cs)

afterMatterP :: Parser AfterMatter
afterMatterP = do
  asInternal <-
    optionMaybe $
      ( ( fromString
            <<< fromCharArray
            <<< A.fromFoldable <$> (char '*' *> many anyDigit)
        ) >>= maybe (fail "Could not parse int")
          ( \v -> case (NEL.fromList $ replicate v unit) of
              Just vv -> pure vv
              Nothing -> fail "Number must be positive"
          )
      )
  pure { asInternal }

tagP :: Parser Tag
tagP = do
  tag <- optionMaybe (map (fromCharArray <<< A.fromFoldable) (char ';' *> many1 alphaNum))
  pure { tag }

weightP :: Parser Number
weightP = do
  _ <- skipSpaces
  add 1.0 <<< toNumber <<< L.length <$> sepEndBy (char '_') whiteSpace1

sampleP :: forall event. Parser (Maybe (Note event))
sampleP = do
  possiblySample <- sampleName
  -- if it is in our original stock, there may be a level
  -- of indirection. for example, chin and chin:0 are the same sample
  -- otherwise, if it is not in the original stock, we use the name as-is
  case O.lookup possiblySample S.nameToSampleMNO of
    Nothing -> pure $ Just $ Note
      { sampleFoT: Right $ Sample possiblySample
      , bufferOffsetFoT: const 0.0
      , rateFoT: const 1.0
      , forward: true
      , volumeFoT: const 1.0
      }
    Just foundSample -> pure foundSample

internalcyclePInternal :: forall event. Parser (ICycle (Maybe (Note event)))
internalcyclePInternal = IInternal <$> do
  nel <- between (skipSpaces *> char '[' *> skipSpaces) (skipSpaces *> char ']' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cyclePInternal unit
    case cyc of
      ISequential { nel } -> pure nel
      xx -> pure (pure xx)
  { tag } <- tagP
  weight <- weightP
  pure { nel, env: { weight, tag } }

branchingcyclePInternal :: forall event. Parser (ICycle (Maybe (Note event)))
branchingcyclePInternal = IBranching <$> do
  nel <- between (skipSpaces *> char '<' *> skipSpaces) (skipSpaces *> char '>' *> skipSpaces) do
    pure unit -- breaks recursion
    cyc <- cyclePInternal unit
    case cyc of
      ISequential { nel } -> pure nel
      xx -> pure (pure xx)
  { tag } <- tagP
  weight <- weightP
  pure { nel, env: { weight, tag } }

simultaneouscyclePInternal :: forall event. Unit -> Parser (ICycle (Maybe (Note event)))
simultaneouscyclePInternal _ = ISimultaneous <<< { env: { weight: 1.0, tag: Nothing }, nel: _ } <$> do
  sep <- sepBy ((fromCharArray <<< A.fromFoldable) <$> many (satisfy (not <<< eq ','))) (string ",")
  case sep of
    Nil -> fail "Lacks comma"
    (_ : Nil) -> fail "Lacks comma"
    (aa : bb : cc) -> case traverse (runParser reducedP) (NonEmptyList (aa :| bb : cc)) of
      Left e -> fail $ show e
      Right rr -> pure rr
  where
  reducedP = try branchingcyclePInternal
    <|> try sequentialcyclePInternal
    <|> try internalcyclePInternal
    <|> try singleSampleP
    <|> fail "Could not parse cycle"

joinSequential :: forall a. List (ICycle a) -> List (ICycle a)
joinSequential Nil = Nil
joinSequential (ISequential { nel: NonEmptyList (aa :| bb) } : cc) = (aa : joinSequential bb) <> joinSequential cc
joinSequential (aa : bb) = aa : joinSequential bb

sequentialcyclePInternal :: forall event. Parser (ICycle (Maybe (Note event)))
sequentialcyclePInternal = ISequential <<< { env: { weight: 1.0, tag: Nothing }, nel: _ } <$> do
  skipSpaces
  leadsWith <- try internalcyclePInternal <|> singleSampleP
  skipSpaces
  rest <- joinSequential <$> many (cyclePInternal unit)
  pure (NonEmptyList (leadsWith :| rest))

singleSampleP :: forall event. Parser (ICycle (Maybe (Note event)))
singleSampleP = do
  skipSpaces
  sample <- sampleP
  afterMatter <- afterMatterP
  skipSpaces
  { tag } <- tagP
  weight <- weightP
  pure $ case afterMatter.asInternal of
    Nothing -> ISingleNote { env: { weight, tag }, val: sample }
    Just ntimes -> IInternal { env: { weight, tag }, nel: map (const $ ISingleNote { env: { weight: 1.0, tag: Nothing }, val: sample }) ntimes }

cyclePInternal :: forall event. Unit -> Parser (ICycle (Maybe (Note event)))
cyclePInternal _ = try branchingcyclePInternal
  <|> try (simultaneouscyclePInternal unit)
  <|> try sequentialcyclePInternal
  <|> try singleSampleP
  <|> fail "Could not parse cycle"

cycleP_ :: Parser (Cycle (Maybe (Note Unit)))
cycleP_ = cycleP

cycleP :: forall event. Parser (Cycle (Maybe (Note event)))
cycleP = go <$> cyclePInternal unit
  where
  go (IBranching { nel: NonEmptyList (a :| Nil) }) = go a
  go (ISimultaneous { nel: NonEmptyList (a :| Nil) }) = go a
  go (ISequential { nel: NonEmptyList (a :| Nil) }) = go a
  go (IInternal { nel: NonEmptyList (a :| Nil) }) = go a
  go (IBranching { env, nel }) = Branching { env, nel: map go nel }
  go (ISimultaneous { env, nel }) = Simultaneous { env, nel: map go nel }
  go (ISequential { env, nel }) = Internal { env, nel: map go nel }
  go (IInternal { env, nel }) = Internal { env, nel: map go nel }
  go (ISingleNote note) = SingleNote note

flatMap :: forall event. NonEmptyList (NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event))))) -> NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event))))
flatMap (NonEmptyList (aa :| Nil)) = aa
flatMap (NonEmptyList (aa :| bb : cc)) = join $ aa # map \a' -> flatMap (wrap (bb :| cc)) # map \bbb -> a' <> bbb

getWeight :: forall a. Cycle a -> Number
getWeight (Branching { env: { weight } }) = weight
getWeight (Simultaneous { env: { weight } }) = weight
getWeight (Internal { env: { weight } }) = weight
getWeight (SingleNote { env: { weight } }) = weight

getTag :: forall a. Cycle a -> Maybe String
getTag (Branching { env: { tag } }) = tag
getTag (Simultaneous { env: { tag } }) = tag
getTag (Internal { env: { tag } }) = tag
getTag (SingleNote { env: { tag } }) = tag

onTagsCWithIndex :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env nel = let nst = ((Set.fromFoldable env.tag) <> st) in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEL.last axc).i cyc))) (pure (go nst ii (NEL.head nel))) (NEL.tail nel) in { i: _.i $ NEL.last folded, val: ff { env, nel: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = case _ of
    Branching { env, nel } -> go' st Branching ii env nel
    Simultaneous { env, nel } -> go' st Simultaneous ii env nel
    Internal { env, nel } -> go' st Internal ii env nel
    SingleNote { env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ SingleNote { env, val } } else { i: ii, val: SingleNote { env, val } }

onTagsC :: forall a. (Set.Set String -> Boolean) -> (Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsC pf f = onTagsCWithIndex pf \_ -> f

onTagsCWithIndex' :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex' pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env nel = let nst = st in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEL.last axc).i cyc))) (pure (go nst ii (NEL.head nel))) (NEL.tail nel) in { i: _.i $ NEL.last folded, val: ff { env, nel: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = case _ of
    Branching { env, nel } -> go' st Branching ii env nel
    Simultaneous { env, nel } -> go' st Simultaneous ii env nel
    Internal { env, nel } -> go' st Internal ii env nel
    SingleNote { env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ SingleNote { env, val } } else { i: ii, val: SingleNote { env, val } }

onTagsC' :: forall a. (Set.Set String -> Boolean) -> (Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsC' pf f = onTagsCWithIndex' pf \_ -> f

onTagsWithIndex :: forall a. (Set.Set String -> Boolean) -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagsWithIndex pf zz = onTagsCWithIndex pf (map <<< zz)

onTagsWithIndex' :: forall a. (Set.Set String -> Boolean) -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagsWithIndex' pf zz = onTagsCWithIndex' pf (map <<< zz)

onTags :: forall a. (Set.Set String -> Boolean) -> (a -> a) -> Cycle a -> Cycle a
onTags pf = onTagsC pf <<< map

onTags' :: forall a. (Set.Set String -> Boolean) -> (a -> a) -> Cycle a -> Cycle a
onTags' pf = onTagsC' pf <<< map

onTagWithIndex :: forall a. String -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagWithIndex = onTagsWithIndex <<< Set.member

onTagWithIndex' :: forall a. String -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagWithIndex' = onTagsWithIndex' <<< Set.member

onTag :: forall a. String -> (a -> a) -> Cycle a -> Cycle a
onTag = onTags <<< Set.member

onTag' :: forall a. String -> (a -> a) -> Cycle a -> Cycle a
onTag' = onTags' <<< Set.member

cycleToSequence :: forall event. CycleDuration -> Cycle (Maybe (Note event)) -> NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event))))
cycleToSequence (CycleDuration cycleDuration) = go { currentSubdivision: cycleDuration, currentOffset: 0.0 }
  where
  go state (Branching { nel }) = join $ map (go state) nel
  go state (Simultaneous { nel }) = map (sortBy (compare `on` (unwrap >>> _.startsAt)))
    $ flatMap
    $ map (go state) nel
  go state (Internal { nel }) = seq state nel
  go state (SingleNote { env: { tag }, val }) = pure $ pure $ NoteInTime
    { duration: state.currentSubdivision
    , startsAt: state.currentOffset
    , note: val
    , cycleDuration
    , tag
    }
  seq state nel =
    let
      allWeights = foldl (+) 0.0 (map getWeight nel)
      withStateInfo = evalState
        ( ( traverse \vv -> do
              ii <- get
              let wt = getWeight vv
              put $ ii + wt
              pure
                { currentSubdivision: state.currentSubdivision * wt / allWeights
                , currentOffset: state.currentOffset + state.currentSubdivision * ii / allWeights
                , vv
                }
          ) nel
        )
        0.0
    in
      flatMap $ map
        (\{ currentSubdivision, currentOffset, vv } -> go { currentSubdivision, currentOffset } vv)
        withStateInfo

unrest :: forall event. NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event)))) -> List (List (NoteInTime (Note event)))
unrest = filter (not <<< L.null) <<< NEL.toList <<< map go
  where
  go =
    filterMap
      ( \(NoteInTime { startsAt, duration, cycleDuration, tag, note }) ->
          NoteInTime <<< { startsAt, duration, cycleDuration, tag, note: _ } <$> note
      ) <<< NEL.toList

asScore :: forall event. Boolean -> NonEmptyList (NoteInFlattenedTime (Note event)) -> NextCycle event
asScore force flattened = NextCycle
  { force
  , samples: Set.fromFoldable $ compact $ NEL.toList $ map (unwrap >>> _.note >>> unwrap >>> _.sampleFoT >>> hush) flattened
  , func: scoreInput
  }
  where
  scoreInput ccPce = go ccPce.currentCount ccPce.prevCycleEnded flattened
  go currentCount prevCycleEnded (NonEmptyList (NoteInFlattenedTime aa :| bb)) =
    let
      st = prevCycleEnded + aa.bigStartsAt
    in
      { startsAfter: st - currentCount
      , rest:
          { sampleFoT: (unwrap aa.note).sampleFoT
          , forward: (unwrap aa.note).forward
          , cycleStartsAt: prevCycleEnded
          , rateFoT: (unwrap aa.note).rateFoT
          , bufferOffsetFoT: (unwrap aa.note).bufferOffsetFoT
          , volumeFoT: (unwrap aa.note).volumeFoT
          , bigCycleDuration: aa.bigCycleDuration
          , littleCycleDuration: aa.littleCycleDuration
          , currentCycle: aa.currentCycle
          , bigStartsAt: aa.bigStartsAt
          , littleStartsAt: aa.littleStartsAt
          , duration: aa.duration
          }
      } :<
        \{ time, headroomInSeconds, input: { next: (NextCycle nc) } } ->
          case bb of
            Nil -> nc.func
              { currentCount: st
              , prevCycleEnded: prevCycleEnded + aa.bigCycleDuration
              , time
              , headroomInSeconds
              }
            cc : dd ->
              if nc.force && aa.positionInCycle == aa.elementsInCycle - 1 then nc.func
                { currentCount: st
                , prevCycleEnded: prevCycleEnded + aa.littleCycleDuration * toNumber (aa.currentCycle + 1)
                , time
                , headroomInSeconds
                }
              else go st prevCycleEnded (NonEmptyList (cc :| dd))

flattenScore :: forall event. NonEmptyList (NonEmptyList (NoteInTime (Note event))) -> NonEmptyList (NoteInFlattenedTime (Note event))
flattenScore l = flattened
  where
  ll = NEL.length l
  flattened = join $ mapWithIndex
    ( \ii -> mapWithIndex
        ( \jj (NoteInTime { note, duration, startsAt, cycleDuration, tag }) -> NoteInFlattenedTime
            { note
            , duration
            , bigStartsAt: startsAt + cycleDuration * toNumber ii
            , currentCycle: ii
            , elementsInCycle: NEL.length (NEL.head l)
            , nCycles: NEL.length l
            , positionInCycle: jj
            , littleStartsAt: startsAt
            , littleCycleDuration: cycleDuration
            , bigCycleDuration: cycleDuration * toNumber ll
            , tag
            }
        )
    )
    l

openVoice :: forall event. Voice event
openVoice = Voice
  { globals: Globals { gain: const 0.0, fx: const calm }
  , next: asScore false (pure intentionalSilenceForInternalUseOnly)
  }

type OpenVoices event = { | EWF (CycleDuration -> Voice event) }

openVoices :: forall event. OpenVoices event
openVoices = hmap (\(_ :: Unit) -> (const $ openVoice)) (mempty :: { | EWF Unit })

type OpenDrones event = { | AH (Maybe (DroneNote event)) }

openDrones :: forall event. OpenDrones event
openDrones = hmap (\(_ :: Unit) -> Nothing) (mempty :: { | AH Unit })

openFuture :: forall event. TheFuture event
openFuture = TheFuture
  $ Record.union (hmap (\(_ :: Unit) -> openVoice) (mempty :: { | EWF Unit }))
  $ Record.union
      (hmap (\(_ :: Unit) -> Nothing) (mempty :: { | AH Unit }))
      { title: "wagsi @ tidal", sounds: (Map.empty :: Map.Map Sample BufferUrl), preload: [] }

reFuture :: forall f event. Foldable f => f Sample -> TheFuture event
reFuture fdbl = set
  ( unto TheFuture
      <<< prop
        (Proxy :: _ "earth")
      <<< unto Voice
      <<< prop (Proxy :: _ "next")
      <<< unto NextCycle
      <<< prop (Proxy :: _ "samples")
  )
  (Set.fromFoldable fdbl)
  openFuture

massiveFuture :: forall event. TheFuture event
massiveFuture = reFuture $ map snd nameToSample

droneyFuture :: forall event. TheFuture event
droneyFuture = reFuture $ map snd dronesToSample

intentionalSilenceForInternalUseOnly :: forall event. (NoteInFlattenedTime (Note event))
intentionalSilenceForInternalUseOnly = NoteInFlattenedTime
  { note: Note
      { sampleFoT: Right $ S.intentionalSilenceForInternalUseOnly__Sample
      , rateFoT: const 1.0
      , forward: true
      , volumeFoT: const 1.0
      , bufferOffsetFoT: const 0.0
      }
  , bigStartsAt: 0.0
  , littleStartsAt: 0.0
  , duration: 0.259253
  , elementsInCycle: 1
  , nCycles: 1
  , positionInCycle: 0
  , currentCycle: 0
  , bigCycleDuration: 0.259253
  , littleCycleDuration: 0.259253
  , tag: Nothing
  }

parse :: forall event. String -> Cycle (Maybe (Note event))
parse = fromMaybe intentionalSilenceForInternalUseOnly_ <<< hush <<< runParser cycleP

parse_ :: String -> Cycle (Maybe (Note Unit))
parse_ = parse

parseInternal :: forall event. String -> CycleDuration -> NextCycle event
parseInternal str dur = asScore false
  $ maybe (pure intentionalSilenceForInternalUseOnly) flattenScore
  $ join
  $ map
      ( NEL.fromList
          <<< compact
          <<< map NEL.fromList
          <<< (unrest <<< cycleToSequence dur)
      )
  $ hush
  $ runParser cycleP str

rend :: forall event. Cycle (Maybe (Note event)) -> CycleDuration -> (NextCycle event)
rend cyn dur = asScore false
  $ maybe (pure intentionalSilenceForInternalUseOnly) flattenScore
  $ NEL.fromList
  $ compact
  $ map NEL.fromList
  $ unrest
  $ cycleToSequence dur
  $ cyn

rend_ :: Cycle (Maybe (Note Unit)) -> CycleDuration -> (NextCycle Unit)
rend_ = rend

rendNit :: forall event. NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event)))) -> NextCycle event
rendNit = asScore false <<< s2f

c2s :: forall event. Cycle (Maybe (Note event)) -> CycleDuration -> NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event))))
c2s = flip cycleToSequence

s2f :: forall event. NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event)))) -> NonEmptyList (NoteInFlattenedTime (Note event))
s2f = maybe (pure intentionalSilenceForInternalUseOnly) flattenScore
  <<< NEL.fromList
  <<< compact
  <<< map NEL.fromList
  <<< unrest

----------------
-- dj quickcheck --

qcSamples :: NonEmptyArray Sample
qcSamples = map fst $ fromMaybe (sampleToDur') $ NEA.fromArray $ NEA.filter ((>) 0.8 <<< snd) sampleToDur

genSingleSample :: Gen Sample
genSingleSample = elements qcSamples

genSingleNote :: forall event. Gen (Cycle (Maybe (Note event)))
genSingleNote =
  SingleNote <<< { env: { weight: 1.0, tag: Nothing }, val: _ } <<< Just <<< Note
    <<<
      { rateFoT: const 1.0
      , volumeFoT: const 1.0
      , forward: true
      , bufferOffsetFoT: const 0.0
      , sampleFoT: _
      }
    <<< Right <$> genSingleSample

genRest :: forall event. Gen (Cycle (Maybe (Note event)))
genRest = pure $ SingleNote { env: { weight: 1.0, tag: Nothing }, val: Nothing }

genSingleNoteOrRest :: forall event. Gen (Cycle (Maybe (Note event)))
genSingleNoteOrRest = frequency (NonEmptyList ((7.0 /\ genSingleNote) :| (3.0 /\ genRest) : Nil))

nea2nel :: NonEmptyArray ~> NonEmptyList
nea2nel = NonEmptyList <<< (\(aa :| bb) -> aa :| L.fromFoldable bb) <<< NEA.toNonEmpty

genCycle :: forall event. Gen (Cycle (Maybe (Note event)))
genCycle = go 0
  where
  genSameInternal nn = do
    sn <- genSingleNote
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 5, 6 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 6 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4 ])
    pure $ Internal { env: { weight: 1.0, tag: Nothing }, nel: nea2nel $ NEA.fromNonEmpty (sn :| A.replicate (ii - 1) sn) }
  genDiffInternal nn = do
    let gg = go (nn + 1)
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 8 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 4 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [])
    nel <- sequence $ nea2nel $ NEA.fromNonEmpty (gg :| A.replicate (ii - 1) gg)
    pure $ Internal { env: { weight: 1.0, tag: Nothing }, nel }
  genBranching nn = resize 3 (Branching <<< { env: { weight: 1.0, tag: Nothing }, nel: _ } <<< nea2nel <$> arrayOf1 (go (nn + 1)))
  genSimultaneous nn = resize 3 (Simultaneous <<< { env: { weight: 1.0, tag: Nothing }, nel: _ } <<< nea2nel <$> arrayOf1 (go (nn + 1)))
  go 0 =
    let
      zz = 0
    in
      frequency
        ( NonEmptyList
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (1.0 /\ (genDiffInternal zz))
                  : Nil
            )
        )
  go 1 =
    let
      zz = 1
    in
      frequency
        ( NonEmptyList
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (2.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (3.0 /\ genSingleNoteOrRest)
                  : Nil
            )
        )
  go 2 =
    let
      zz = 2
    in
      frequency
        ( NonEmptyList
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (8.0 /\ genSingleNoteOrRest)
                  : Nil
            )
        )
  go 3 =
    let
      zz = 3
    in
      frequency
        ( NonEmptyList
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ genSimultaneous zz)
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (10.0 /\ genSingleNoteOrRest)
                  : Nil
            )
        )
  go _ = genSingleNoteOrRest

genVoice :: forall event. Number -> Gen { cycle :: Cycle (Maybe (Note event)), voice :: Voice event }
genVoice vol = do
  let globals = Globals { gain: const vol, fx: const calm }
  cycle <- genCycle
  cl' <- arbitrary
  let cl = CycleDuration (1.0 + 3.0 * cl')
  let next = asScore false $ s2f $ c2s cycle cl
  pure $ { cycle, voice: Voice { globals, next } }

djQuickCheck :: forall event. Gen { cycle :: Cycle (Maybe (Note event)), future :: TheFuture event }
djQuickCheck = do
  { cycle, voice: earth } <- genVoice 1.0
  wind <- pure openVoice
  fire <- pure openVoice
  pure $
    { cycle
    , future: TheFuture
        { earth
        , wind
        , fire
        , air: Nothing
        , heart: Nothing
        , title: "d j q u i c k c h e c k"
        , sounds: Map.empty
        , preload: []
        }
    }

class S s event where
  s :: s -> CycleDuration -> Voice event

instance sString :: S String event where
  s = map plainly <<< parseInternal

instance sCycle :: S (Cycle (Maybe (Note event))) event where
  s = map plainly <<< rend

instance sNit :: S (NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event))))) event where
  s = map plainly <<< pure <<< rendNit

instance sNitFT :: S (CycleDuration -> (NonEmptyList (NonEmptyList (NoteInTime (Maybe (Note event)))))) event where
  s = map plainly <<< map rendNit

instance sNift :: S (NonEmptyList (NoteInFlattenedTime (Note event))) event where
  s = map plainly <<< pure <<< asScore false

instance sNiftFT :: S (CycleDuration -> (NonEmptyList (NoteInFlattenedTime (Note event)))) event where
  s = map plainly <<< map (asScore false)

instance sNextCycle :: S (NextCycle event) event where
  s = map plainly <<< pure

instance sNextCycleFT :: S (CycleDuration -> NextCycle event) event where
  s = map plainly

instance sVoice :: S (Voice event) event where
  s = const

instance sVoiceFT :: S (CycleDuration -> (Voice event)) event where
  s = identity

betwixt :: forall n. Ord n => n -> n -> n -> n
betwixt mn' mx' n = if n < mn then mn else if n > mx then mx else n
  where
  mn = min mn' mx'
  mx = max mn' mx'

derivative :: forall a. ClockTime a => (TimeIsAndWas a -> Number) -> Number -> TimeIsAndWas a -> Number
derivative f y v = fromMaybe' (\_ -> f v) do
  let v' = unwrap v
  let ti = v'.timeIs
  mti' <- v'.timeWas
  mn' <- v'.valWas
  pure $ (y * (clockTime ti - clockTime mti')) + mn'
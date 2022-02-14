module WAGS.Lib.Tidal.Tidal
  ( addDroneEffect
  , addEffect
  , asScore
  , b
  , b'
  , betwixt
  , c2s
  , numericTumult
  , changeBufferOffset
  , changeDroneForward
  , changeDroneLoopEnd
  , changeDroneLoopStart
  , changeDroneRate
  , changeDroneVolume
  , changeEffect
  , changeForward
  , changeRate
  , changeSample
  , changeSampleF
  , changeVolume
  , class ParseToCycle
  , class S
  , cycleP
  , djQuickCheck
  , drone
  , fadeTo
  , focus
  , i
  , i'
  , ident
  , impatient
  , intentionalSilenceForInternalUseOnly
  , l_j
  , l_r
  , lct
  , lcw
  , ldf
  , ldle
  , ldls
  , ldr
  , lds
  , ldt
  , ldv
  , lfb
  , lfc
  , lfd
  , lfl
  , lfn
  , lft
  , lnbo
  , lnf
  , lnr
  , lns
  , lnv
  , lnx
  , ltd
  , ltn
  , lts
  , ltt
  , lvt
  , make
  , module WAGS.Lib.Tidal.Cycle
  , mseq
  , n
  , nefy
  , nl
  , onTag
  , onTag'
  , onTagWithIndex
  , onTagWithIndex'
  , onTags
  , onTags'
  , onTagsC
  , onTagsC'
  , onTagsCWithIndex
  , onTagsCWithIndex'
  , onTagsWithIndex
  , onTagsWithIndex'
  , openFuture
  , openVoice
  , oscWarp
  , parse
  , parseWithBrackets
  , plainly
  , rend
  , rendNit
  , s
  , s2f
  , sequentialcyclePInternal
  , unrest
  , when_
  , x
  , x'
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Control.Monad.State (evalState, get, put)
import Data.Array (fold, null, uncons, (..))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..), either, hush)
import Data.Filterable (compact, filter, filterMap, maybeBool)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Identity (Identity(..))
import Data.Int (fromString, toNumber)
import Data.Lens (Lens', Prism', _Left, _Right, iso, lens, over, prism', set, traversed)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List (List(..), foldMap, foldl, (:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set as Set
import Data.String.CodeUnits as CU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D1)
import Data.Unfoldable (replicate)
import Data.Variant (Variant, prj, match)
import Data.Variant.Either (right)
import Data.Variant.Either as VE
import Data.Variant.Maybe (just, nothing)
import Data.Variant.Maybe as VM
import Data.Vec ((+>))
import Data.Vec as V
import Foreign.Object (Object)
import Foreign.Object as O
import Foreign.Object as Object
import Math (exp, pow)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record as Record
import Safe.Coerce (coerce)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf1, elements, frequency, resize)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodeUnits (alphaNum, anyDigit, char, oneOf, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (between, many, many1, optionMaybe, sepBy1, sepEndBy, sepEndBy1)
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))
import WAGS.Create (class Create)
import WAGS.Create.Optionals (input)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Lib.HList (HCons(..), HNil)
import WAGS.Lib.Tidal.Cycle (Cycle(..), singleton, branching, simultaneous, internal, flattenCycle, intentionalSilenceForInternalUseOnly_, reverse)
import WAGS.Lib.Tidal.Samples (sample2drone, urls)
import Prim.Row as Row
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.TLP (class MiniNotation)
import WAGS.Lib.Tidal.Types (AfterMatter, BFoT, BFoT', BufferUrl, ClockTimeIs', CycleDuration(..), DroneNote(..), EWF, EWF', FXInput', FoT, Globals(..), NextCycle(..), Note(..), Note', NoteInFlattenedTime(..), NoteInTime(..), NoteLazy', O'Fx, O'Past, Sample(..), Tag, TheFuture(..), TimeIs, TimeIs', UnsampledTimeIs, Voice(..), WH, WH', getNow)
import WAGS.Rendered (Instruction')
import WAGS.Tumult (Tumultuous, safeUntumult)
import WAGS.Tumult.Make (tumultuously)
import WAGS.Validation (class NodesCanBeTumultuous, class SubgraphIsRenderable)

-- | Only play the first cycle, and truncate/interrupt the playing cycle at the next sub-ending.
impatient :: NextCycle ~> NextCycle
impatient = over (unto NextCycle <<< prop (Proxy :: _ "force")) (const true)

make
  :: forall inRec overfull rest event
   . Union inRec
       ( EWF' (CycleDuration -> Voice event)
           ( WH' (VM.Maybe (DroneNote event))
               ( title :: String
               , sounds :: Object BufferUrl
               , preload :: Array Sample
               )
           )
       )
       overfull
  => Nub overfull
       ( EWF' (CycleDuration -> Voice event)
           ( WH' (VM.Maybe (DroneNote event))
               ( title :: String
               , sounds :: Object BufferUrl
               , preload :: Array Sample
               | rest
               )
           )
       )
  => Number
  -> { | inRec }
  -> TheFuture event
make cl rr = TheFuture $ Record.union
  ( fromHomogeneous $ map ((#) cycleDuration) $ homogeneous
      { earth: z.earth
      , wind: z.wind
      , fire: z.fire
      , lambert: z.lambert
      , hendricks: z.hendricks
      , ross: z.ross
      }
  )
  { water: z.water
  , heart: z.heart
  , title: z.title
  , sounds: z.sounds
  , preload: z.preload
  , cycleDuration
  }
  where
  cycleDuration = wrap cl
  z =
    Record.merge rr
      ( Record.union (openVoices :: OpenVoices event)
          $ Record.union (openDrones :: OpenDrones event)
              { title: "wagsi @ tidal"
              , preload: [] :: Array Sample
              , sounds: Object.empty :: Object BufferUrl
              }
      )
      ::
           { | EWF' (CycleDuration -> Voice event)
               ( WH' (VM.Maybe (DroneNote event))
                   ( title :: String
                   , sounds :: Object BufferUrl
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
plainly = Voice <<< { globals: Globals { fx: const calm }, next: _ }

--- lenses
l_j :: forall a. Prism' (VM.Maybe a) a
l_j = prism' VM.just (VM.maybe Nothing Just)

l_r :: forall a b. Prism' (VE.Either a b) b
l_r = prism' VE.right (VM.maybe Nothing Just <<< VE.hush)

lvt :: forall event. Lens' (Voice event) (O'Fx event)
lvt = unto Voice <<< prop (Proxy :: _ "globals") <<< unto Globals <<< prop (Proxy :: _ "fx")

addEffect
  :: forall event
   . (FXInput' event -> Tumultuous D1 "output" (voice :: Unit))
  -> Voice event
  -> Voice event
addEffect = set lvt <<< lcmap (getNow >>> unwrap)

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
lft = unto NoteInFlattenedTime <<< prop (Proxy :: _ "tag") <<< l_j

ltn :: forall note. Lens' (NoteInTime note) note
ltn = unto NoteInTime <<< prop (Proxy :: _ "note")

lts :: forall note. Lens' (NoteInTime note) Number
lts = unto NoteInTime <<< prop (Proxy :: _ "startsAt")

ltd :: forall note. Lens' (NoteInTime note) Number
ltd = unto NoteInTime <<< prop (Proxy :: _ "duration")

ltt :: forall p note. Choice p => Strong p => p String String -> p (NoteInTime note) (NoteInTime note)
ltt = unto NoteInTime <<< prop (Proxy :: _ "tag") <<< l_j

setter' :: forall t163 t170 t171 t172 t175 t176 t177 t178. Traversable t170 => Profunctor t177 => Newtype t178 t175 => ((t163 -> t177 t178 t176) -> t171 -> t172) -> t177 t175 t176 -> t170 t171 -> t170 t172
setter' len = set (traversed <<< len) <<< lcmap unwrap

lns :: forall event. Lens' (Note event) (VE.Either (UnsampledTimeIs event -> Sample) Sample)
lns = unto Note <<< prop (Proxy :: _ "sampleFoT")

changeSample
  :: forall container event
   . Traversable container
  => Sample
  -> container (Note event)
  -> container (Note event)
changeSample = set (traversed <<< lns <<< iso (VE.either Left Right) (either VE.left VE.right) <<< _Right)

changeSampleF
  :: forall container event
   . Traversable container
  => (UnsampledTimeIs event -> Sample)
  -> container (Note event)
  -> container (Note event)
changeSampleF = set (traversed <<< lns <<< iso (VE.either Left Right) (either VE.left VE.right) <<< _Left)

lnr :: forall event. Lens' (Note event) (FoT event)
lnr = unto Note <<< prop (Proxy :: _ "rateFoT")

type ChangeSig =
  forall container event
   . Traversable container
  => (TimeIs' event -> Number)
  -> container (Note event)
  -> container (Note event)

changeRate :: ChangeSig
changeRate = setter' lnr

lnx :: forall event. Lens' (Note event) (TimeIs event -> Tumultuous D1 "output" (voice :: Unit))
lnx = unto Note <<< prop (Proxy :: _ "tumultFoT")

changeEffect
  :: forall container event
   . Traversable container
  => (TimeIs' event -> Tumultuous D1 "output" (voice :: Unit))
  -> container (Note event)
  -> container (Note event)
changeEffect = setter' lnx

lnbo :: forall event. Lens' (Note event) (FoT event)
lnbo = unto Note <<< prop (Proxy :: _ "bufferOffsetFoT")

changeBufferOffset :: ChangeSig
changeBufferOffset = setter' lnbo

lnf :: forall event. Lens' (Note event) (BFoT event)
lnf = unto Note <<< prop (Proxy :: _ "forwardFoT")

changeForward
  :: forall container event
   . Traversable container
  => BFoT' event
  -> container (Note event)
  -> container (Note event)
changeForward = set (traversed <<< lnf) <<< lcmap unwrap

lnv :: forall event. Lens' (Note event) (FoT event)
lnv = unto Note <<< prop (Proxy :: _ "volumeFoT")

changeVolume :: ChangeSig
changeVolume = setter' lnv

type ChangeDroneSig =
  forall event
   . (ClockTimeIs' event -> Number)
  -> DroneNote event
  -> DroneNote event

lds :: forall event. Lens' (DroneNote event) Sample
lds = unto DroneNote <<< prop (Proxy :: _ "sample")

ldr :: forall event. Lens' (DroneNote event) (O'Past event)
ldr = unto DroneNote <<< prop (Proxy :: _ "rateFoT")

droneSetter'
  :: forall t3227 t3228 t3229 t3233 t3234 t3236 t3238 t3239 t3240 t3241
   . Newtype t3234 t3233
  => Newtype t3236
       ( Variant
           ( timeIs ::
               { timeIs :: t3234
               | t3238
               }
           , timeIsAndWas ::
               { timeIs :: t3234
               | t3239
               }
           , timeIsAndWasAndHadBeen ::
               { timeIs :: t3234
               | t3240
               }
           )
       )
  => ((t3229 -> t3236 -> t3241) -> t3227 -> t3228)
  -> (t3233 -> t3241)
  -> t3227
  -> t3228
droneSetter' xx ff = set xx (ff <<< unwrap <<< getNow)

changeDroneRate :: ChangeDroneSig
changeDroneRate = droneSetter' ldr

ldls :: forall event. Lens' (DroneNote event) (O'Past event)
ldls = unto DroneNote <<< prop (Proxy :: _ "loopStartFoT")

changeDroneLoopStart :: ChangeDroneSig
changeDroneLoopStart = droneSetter' ldls

ldle :: forall event. Lens' (DroneNote event) (O'Past event)
ldle = unto DroneNote <<< prop (Proxy :: _ "loopEndFoT")

changeDroneLoopEnd :: ChangeDroneSig
changeDroneLoopEnd = droneSetter' ldle

ldf :: forall event. Lens' (DroneNote event) Boolean
ldf = unto DroneNote <<< prop (Proxy :: _ "forward")

changeDroneForward
  :: forall event
   . Boolean
  -> DroneNote event
  -> DroneNote event
changeDroneForward = set ldf

ldv :: forall event. Lens' (DroneNote event) (O'Past event)
ldv = unto DroneNote <<< prop (Proxy :: _ "volumeFoT")

changeDroneVolume :: ChangeDroneSig
changeDroneVolume = droneSetter' ldv

ldt :: forall event. Lens' (DroneNote event) (O'Fx event)
ldt = unto DroneNote <<< prop (Proxy :: _ "tumultFoT")

addDroneEffect
  :: forall event
   . (FXInput' event -> Tumultuous D1 "output" (voice :: Unit))
  -> DroneNote event
  -> DroneNote event
addDroneEffect = set ldt <<< lcmap (getNow >>> unwrap)

lcw :: forall note. Lens' (Cycle note) Number
lcw = lens getWeight
  ( unwrap >>> match
      { branching: \ii -> \weight -> branching $ ii { env = ii.env { weight = weight } }
      , simultaneous: \ii -> \weight -> simultaneous $ ii { env = ii.env { weight = weight } }
      , internal: \ii -> \weight -> internal $ ii { env = ii.env { weight = weight } }
      , singleton: \ii -> \weight -> singleton $ ii { env = ii.env { weight = weight } }
      }
  )

lct :: forall note. Lens' (Cycle note) (VM.Maybe String)
lct = lens getTag
  ( unwrap >>> match
      { branching: \ii -> \tag -> branching $ ii { env = ii.env { tag = tag } }
      , simultaneous: \ii -> \tag -> simultaneous $ ii { env = ii.env { tag = tag } }
      , internal: \ii -> \tag -> internal $ ii { env = ii.env { tag = tag } }
      , singleton: \ii -> \tag -> singleton $ ii { env = ii.env { tag = tag } }
      }
  )

---

when_ :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
when_ cond func aa = if cond aa then func aa else aa

focus :: forall a. (a -> Boolean) -> Prism' a a
focus = prism' identity <<< maybeBool

---
b :: forall event. Cycle (VM.Maybe (Note event)) -> Array (Cycle (VM.Maybe (Note event))) -> Cycle (VM.Maybe (Note event))
b bx by = branching { env: { weight: 1.0, tag: VM.nothing }, cycles: NEA.fromNonEmpty (bx :| by) }

b' :: forall event. Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event))
b' bx = b bx []

nefy :: forall f a b. (a -> f a -> b) -> NonEmpty f a -> b
nefy ff (xx :| yy) = ff xx yy

class H2N a b | a -> b where
  h2n :: a -> NonEmpty Array b

instance h2nTuple :: TypeEquals a aa => H2N (HCons a HNil) aa where
  h2n (HCons aa _) = coerce (proof :: Identity a -> Identity aa) aa :| []
else instance h2nTuple2 :: (TypeEquals a aa, H2N b aa) => H2N (HCons a b) aa where
  h2n (HCons aa bb) = coerce (proof :: Identity a -> Identity aa) aa :| [ qq ] <> rr
    where
    (qq :| rr) = h2n bb

i :: forall event. Cycle (VM.Maybe (Note event)) -> Array (Cycle (VM.Maybe (Note event))) -> Cycle (VM.Maybe (Note event))
i sx sy = internal { env: { weight: 1.0, tag: VM.nothing }, cycles: NEA.fromNonEmpty (sx :| sy) }

i' :: forall event. Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event))
i' sx = i sx []

x :: forall event. Cycle (VM.Maybe (Note event)) -> Array (Cycle (VM.Maybe (Note event))) -> Cycle (VM.Maybe (Note event))
x xx xy = simultaneous { env: { weight: 1.0, tag: VM.nothing }, cycles: NEA.fromNonEmpty (xx :| xy) }

x' :: forall event. Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event))
x' sx = x sx []

drone :: forall event. String -> VM.Maybe (DroneNote event)
drone = VM.just <<< sample2drone <<< Sample

nel2nea :: NonEmptyList ~> NonEmptyArray
nel2nea (NonEmptyList (aa :| bb)) = NEA.fromNonEmpty (aa :| A.fromFoldable bb)

sampleName :: Parser String
sampleName = map (CU.fromCharArray <<< NEA.toArray <<< nel2nea) (many1 $ oneOf [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', '~' ])

---
whiteSpace1 :: Parser String
whiteSpace1 = do
  cs <- many1 (satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap CU.singleton cs)

afterMatterP :: Parser AfterMatter
afterMatterP = do
  asInternal <-
    map (maybe VM.nothing VM.just) $ optionMaybe $
      ( ( fromString
            <<< CU.fromCharArray
            <<< A.fromFoldable <$> (char '*' *> many anyDigit)
        ) >>= maybe (fail "Could not parse int")
          ( \v -> case (NEA.fromArray $ replicate v unit) of
              Just vv -> pure vv
              Nothing -> fail "Number must be positive"
          )
      )
  pure { asInternal }

tagP :: Parser Tag
tagP = do
  tag <- optionMaybe (map (CU.fromCharArray <<< A.fromFoldable) (char ';' *> many1 alphaNum))
  pure { tag: maybe VM.nothing VM.just tag }

weightP :: Parser Number
weightP = do
  _ <- skipSpaces
  add 1.0 <<< toNumber <<< L.length <$> sepEndBy (char '_') whiteSpace1

sampleP :: forall event. Parser (VM.Maybe (Note event))
sampleP = do
  ourSample <- sampleName
  pure $
    if ourSample == "~" then nothing
    else just $ Note
      { sampleFoT: VE.right $ Sample ourSample
      , bufferOffsetFoT: const 0.0
      , rateFoT: const 1.0
      , forwardFoT: const true
      , volumeFoT: const 1.0
      , tumultFoT: const (fx $ goodbye hello)
      }

branchingcyclePInternal :: forall event. Int -> Parser (Cycle (VM.Maybe (Note event)))
branchingcyclePInternal lvl = branching <$> do
  cycles <- between (skipSpaces *> char '<' *> skipSpaces) (skipSpaces *> char '>' *> skipSpaces) do
    pure unit -- breaks recursion
    --  = spy (ident i <> "branchingcyclePInternal in between " <> show i) true
    map nel2nea $ sepBy1 (cyclePInternal (lvl + 1)) skipSpaces
  --  = spy (ident i <> "branchingcyclePInternal finished" <> show i) cycles
  --------- HACK
  --- if there is a single sequential we expand it
  { tag } <- tagP
  weight <- weightP
  pure { cycles, env: { weight, tag } }

ident ∷ Int → String
ident lvl = (fold (map (const " ") (0 .. (lvl - 1))))

simultaneouscyclePInternal :: forall event. Int -> Parser (Cycle (VM.Maybe (Note event)))
simultaneouscyclePInternal lvl = simultaneous <$> do
  cycles <- between (skipSpaces *> char '[' *> skipSpaces) (skipSpaces *> char ']' *> skipSpaces) do
    pure unit -- breaks recursion
    --  = spy (ident i <> "simultaneouscyclePInternal in between " <> show i) true
    map nel2nea $ sepBy1 (sequentialcyclePInternal (lvl + 1)) (skipSpaces *> char ',' *> skipSpaces)
  --  = spy (ident i <> "simultaneouscyclePInternal finishing " <> show i) true
  { tag } <- tagP
  weight <- weightP
  --  = spy (ident i <> "simultaneouscyclePInternal got weights " <> show i) true
  pure { cycles, env: { weight, tag } }

sequentialcyclePInternal :: forall event. Int -> Parser (Cycle (VM.Maybe (Note event)))
sequentialcyclePInternal lvl = map internal do
  pure unit -- breaks recursion
  --  = spy (ident i <> "rewinding to sequentialcyclePInternal starting many " <> show i) true
  cycles <- skipSpaces *> (map nel2nea $ sepEndBy1 (cyclePInternal (lvl + 1)) skipSpaces)
  --  = spy (ident i <> "rewinding to sequentialcyclePInternal ending many with length " <> (show $ NEA.length cycles) <> " " <> show i) true
  pure { cycles, env: { weight: 1.0, tag: VM.nothing } }

singleSampleP :: forall event. Int -> Parser (Cycle (VM.Maybe (Note event)))
singleSampleP _ = do
  skipSpaces
  sample <- sampleP
  afterMatter <- afterMatterP
  skipSpaces
  { tag } <- tagP
  weight <- weightP
  pure $ VM.maybe' (\_ -> singleton { env: { weight, tag }, val: sample }) (\ntimes -> internal { env: { weight, tag }, cycles: map (const $ singleton { env: { weight: 1.0, tag: VM.nothing }, val: sample }) ntimes }) afterMatter.asInternal

cyclePInternal :: forall event. Int -> Parser (Cycle (VM.Maybe (Note event)))
cyclePInternal lvl = try (branchingcyclePInternal lvl)
  <|> try (simultaneouscyclePInternal lvl)
  <|> try (singleSampleP lvl)
  <|> fail "Could not parse cycle"

cycleP :: forall event. Parser (Cycle (VM.Maybe (Note event)))
cycleP = go <$> cyclePInternal 0
  where
  c cycles env ctr =
    let
      h = NEA.head cycles
      t = NEA.tail cycles
    in
      if null t then go h else ctr { env, cycles: map go cycles }
  go = unwrap >>> match
    { branching: \{ cycles, env } -> c cycles env branching
    , simultaneous: \{ cycles, env } -> c cycles env simultaneous
    , internal: \{ cycles, env } -> c cycles env internal
    , singleton: \note -> singleton note
    }

flatMap :: forall event. NonEmptyArray (NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event))))) -> NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event))))
flatMap nea =
  let
    aa = NEA.head nea
    tt = NEA.tail nea
    ucd = uncons tt
  in
    ucd # maybe aa \ucd' ->
      let
        bb = ucd'.head
        cc = ucd'.tail
      in
        join $ aa # map \a' -> flatMap (NEA.fromNonEmpty (bb :| cc)) # map \bbb -> a' <> bbb

getWeight :: forall a. Cycle a -> Number
getWeight = unwrap >>> match
  { branching: _.env >>> _.weight
  , simultaneous: _.env >>> _.weight
  , internal: _.env >>> _.weight
  , singleton: _.env >>> _.weight
  }

getTag :: forall a. Cycle a -> VM.Maybe String
getTag = unwrap >>> match
  { branching: _.env >>> _.tag
  , simultaneous: _.env >>> _.tag
  , internal: _.env >>> _.tag
  , singleton: _.env >>> _.tag
  }

onTagsCWithIndex :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env cycles = let nst = ((Set.fromFoldable env.tag) <> st) in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEA.last axc).i cyc))) (pure (go nst ii (NEA.head cycles))) (NEA.tail cycles) in { i: _.i $ NEA.last folded, val: ff { env, cycles: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = unwrap >>> match
    { branching: \{ env, cycles } -> go' st branching ii env cycles
    , simultaneous: \{ env, cycles } -> go' st simultaneous ii env cycles
    , internal: \{ env, cycles } -> go' st internal ii env cycles
    , singleton: \{ env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ singleton { env, val } } else { i: ii, val: singleton { env, val } }
    }

onTagsC :: forall a. (Set.Set String -> Boolean) -> (Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsC pf f = onTagsCWithIndex pf \_ -> f

onTagsCWithIndex' :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex' pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env cycles = let nst = st in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEA.last axc).i cyc))) (pure (go nst ii (NEA.head cycles))) (NEA.tail cycles) in { i: _.i $ NEA.last folded, val: ff { env, cycles: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = unwrap >>> match
    { branching: \{ env, cycles } -> go' st branching ii env cycles
    , simultaneous: \{ env, cycles } -> go' st simultaneous ii env cycles
    , internal: \{ env, cycles } -> go' st internal ii env cycles
    , singleton: \{ env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ singleton { env, val } } else { i: ii, val: singleton { env, val } }
    }

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

cycleToSequence :: forall event. CycleDuration -> Cycle (VM.Maybe (Note event)) -> NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event))))
cycleToSequence (CycleDuration cycleDuration) = go { currentSubdivision: cycleDuration, currentOffset: 0.0 }
  where
  go state = unwrap >>> match
    { branching: \{ cycles } -> join $ map (go state) cycles
    , simultaneous: \{ cycles } -> map (NEA.sortBy (compare `on` (unwrap >>> _.startsAt)))
        $ flatMap
        $ map (go state) cycles
    , internal: \{ cycles } -> seq state cycles
    , singleton: \{ env: { tag }, val } -> pure $ pure $ NoteInTime
        { duration: state.currentSubdivision
        , startsAt: state.currentOffset
        , note: val
        , cycleDuration
        , tag
        }
    }
  seq state cycles =
    let
      allWeights = foldl (+) 0.0 (map getWeight cycles)
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
          ) cycles
        )
        0.0
    in
      flatMap $ map
        (\{ currentSubdivision, currentOffset, vv } -> go { currentSubdivision, currentOffset } vv)
        withStateInfo

unrest :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event)))) -> Array (Array (NoteInTime (Note event)))
unrest = filter (not <<< A.null) <<< NEA.toArray <<< map go
  where
  go =
    filterMap
      ( \(NoteInTime { startsAt, duration, cycleDuration, tag, note }) ->
          VM.maybe Nothing Just $ map (NoteInTime <<< { startsAt, duration, cycleDuration, tag, note: _ }) note
      ) <<< NEA.toArray

asScore :: forall event. Boolean -> NonEmptyArray (NoteInFlattenedTime (Note event)) -> NextCycle event
asScore force flattened = NextCycle
  { force
  , samples: compact $ NEA.toArray $ map (unwrap >>> _.note >>> unwrap >>> _.sampleFoT >>> VE.hush >>> VM.maybe Nothing Just) flattened
  , func: scoreInput
  }
  where
  scoreInput ccPce = go ccPce.currentCount ccPce.prevCycleEnded flattened
  go currentCount prevCycleEnded nea =
    let
      NoteInFlattenedTime aa :| bb = NEA.toNonEmpty nea
      st = prevCycleEnded + aa.bigStartsAt
    in
      { startsAfter: st - currentCount
      , rest:
          { sampleFoT: (unwrap aa.note).sampleFoT
          , forwardFoT: (unwrap aa.note).forwardFoT
          , cycleStartsAt: prevCycleEnded
          , rateFoT: (unwrap aa.note).rateFoT
          , tumultFoT: (unwrap aa.note).tumultFoT
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
          case uncons bb of
            Nothing -> nc.func
              { currentCount: st
              , prevCycleEnded: prevCycleEnded + aa.bigCycleDuration
              , time
              , headroomInSeconds
              }
            Just { head: cc, tail: dd } ->
              if nc.force && aa.positionInCycle == aa.elementsInCycle - 1 then nc.func
                { currentCount: st
                , prevCycleEnded: prevCycleEnded + aa.littleCycleDuration * toNumber (aa.currentCycle + 1)
                , time
                , headroomInSeconds
                }
              else go st prevCycleEnded (NEA.fromNonEmpty (cc :| dd))

flattenScore :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (Note event))) -> NonEmptyArray (NoteInFlattenedTime (Note event))
flattenScore l = flattened
  where
  ll = NEA.length l
  flattened = join $ mapWithIndex
    ( \ii -> mapWithIndex
        ( \jj (NoteInTime { note, duration, startsAt, cycleDuration, tag }) -> NoteInFlattenedTime
            { note
            , duration
            , bigStartsAt: startsAt + cycleDuration * toNumber ii
            , currentCycle: ii
            , elementsInCycle: NEA.length (NEA.head l)
            , nCycles: NEA.length l
            , positionInCycle: jj
            , littleStartsAt: startsAt
            , littleCycleDuration: cycleDuration
            , bigCycleDuration: cycleDuration * toNumber ll
            , tag
            }
        )
    )
    l

openVoice :: forall event. CycleDuration -> Voice event
openVoice = Voice
  <<<
    { globals: Globals { fx: const calm }
    , next: _
    }
  <<< asScore false
  <<< pure
  <<< intentionalSilenceForInternalUseOnly

type OpenVoices event = { | EWF (CycleDuration -> Voice event) }

openVoices :: forall event. OpenVoices event
openVoices = fromHomogeneous
  $ map
      ( const $ s
          ( intentionalSilenceForInternalUseOnly_
              :: Cycle (VM.Maybe (Note event))
          )
      )
  $ homogeneous (mempty :: { | EWF Unit })

type OpenDrones event = { | WH (VM.Maybe (DroneNote event)) }

openDrones :: forall event. OpenDrones event
openDrones = fromHomogeneous
  $ map (const VM.nothing)
  $ homogeneous (mempty :: { | WH Unit })

openFuture :: forall event. CycleDuration -> TheFuture event
openFuture cycleDuration = TheFuture
  $ Record.union
      ( fromHomogeneous $ map (const (openVoice cycleDuration))
          $ homogeneous (mempty :: { | EWF Unit })
      )
  $ Record.union
      ( fromHomogeneous $ map (const VM.nothing)
          $ homogeneous (mempty :: { | WH Unit })
      )
      { title: "wagsi @ tidal"
      , cycleDuration
      , sounds: (Object.empty :: Object BufferUrl)
      , preload: []
      }

intentionalSilenceForInternalUseOnly
  :: forall event. CycleDuration -> NoteInFlattenedTime (Note event)
intentionalSilenceForInternalUseOnly (CycleDuration cl) = NoteInFlattenedTime
  { note: Note
      { sampleFoT: VE.right $ S.intentionalSilenceForInternalUseOnly__Sample
      , rateFoT: const 1.0
      , forwardFoT: const true
      , volumeFoT: const 1.0
      , tumultFoT: const (fx $ goodbye hello)
      , bufferOffsetFoT: const 0.0
      }
  , bigStartsAt: 0.0
  , littleStartsAt: 0.0
  , duration: cl
  , elementsInCycle: 1
  , nCycles: 1
  , positionInCycle: 0
  , currentCycle: 0
  , bigCycleDuration: cl
  , littleCycleDuration: cl
  , tag: VM.nothing
  }

parseWithBrackets
  :: forall event
   . String
  -> Either
       { error :: String
       , pos :: Int
       }
       (Cycle (VM.Maybe (Note event)))
parseWithBrackets = runParser cycleP
  <<< ("[ " <> _)
  <<< (_ <> " ]")

class ParseToCycle s where
  parse :: forall event. s -> Cycle (VM.Maybe (Note event))

instance parseToCycleString :: ParseToCycle String where
  parse = fromMaybe intentionalSilenceForInternalUseOnly_
    <<< hush
    <<< parseWithBrackets

instance parseToCycleProxy :: (IsSymbol sym, MiniNotation sym) => ParseToCycle (Proxy sym) where
  parse = parse <<< reflectSymbol

rend :: forall event. Cycle (VM.Maybe (Note event)) -> CycleDuration -> (NextCycle event)
rend cyn dur = asScore false
  $ maybe (pure (intentionalSilenceForInternalUseOnly dur)) flattenScore
  $ NEA.fromArray
  $ compact
  $ map NEA.fromArray
  $ unrest
  $ cycleToSequence dur
  $ cyn

rendNit :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event)))) -> NextCycle event
rendNit = asScore false <<< s2f

c2s :: forall event. Cycle (VM.Maybe (Note event)) -> CycleDuration -> NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event))))
c2s = flip cycleToSequence

n :: forall event. (Note' event -> Note' event) -> Note event
n f = Note $ f
  { sampleFoT: right S.intentionalSilenceForInternalUseOnly__Sample
  , forwardFoT: const true
  , rateFoT: pure 1.0
  , bufferOffsetFoT: pure 0.0
  , tumultFoT: const (fx $ goodbye hello)
  , volumeFoT: pure 1.0
  }

nl :: forall event. (NoteLazy' event -> NoteLazy' event) -> Note event
nl = n <<< dimap
  ( \iii ->
      { s: "intentionalSilenceForInternalUseOnly"
      , f: iii.forwardFoT
      , r: iii.rateFoT
      , b: iii.bufferOffsetFoT
      , v: iii.volumeFoT
      , t: iii.tumultFoT
      }
  )
  ( \iii ->
      { sampleFoT: right $ wrap iii.s
      , forwardFoT: iii.f
      , rateFoT: iii.r
      , bufferOffsetFoT: iii.b
      , volumeFoT: iii.v
      , tumultFoT: iii.t
      }
  )

mseq :: forall event. Number -> NonEmpty Array (Number /\ Note event) -> NonEmptyArray (NoteInFlattenedTime (Note event))
mseq dur ii' = oo
  where
  ii = NEA.fromNonEmpty ii'
  lenny = NEA.length ii
  sorted = NEA.sortBy (compare `on` fst) ii
  dury = max dur (0.1 + (fst $ NEA.last sorted))
  d0 = map fst sorted
  d1 = maybe (NEA.fromNonEmpty (dury :| [])) (flip NEA.snoc dury) (NEA.fromArray (NEA.drop 1 d0))
  diffs = NEA.zipWith sub d1 d0
  oo = mapWithIndex
    ( \iii ((dd /\ nn) /\ dff) -> NoteInFlattenedTime
        { note: nn
        , bigStartsAt: dd
        , littleStartsAt: dd
        , currentCycle: 0
        , positionInCycle: iii
        , elementsInCycle: lenny
        , nCycles: 1
        , duration: dff
        , bigCycleDuration: dury
        , littleCycleDuration: dury
        , tag: nothing
        }
    )
    (NEA.zipWith (/\) sorted diffs)

s2f
  :: forall event
   . NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note event))))
  -> NonEmptyArray (NoteInFlattenedTime (Note event))
s2f = fromMaybe
  <$>
    ( pure <<< intentionalSilenceForInternalUseOnly
        <<< CycleDuration
        <<< _.cycleDuration
        <<< unwrap
        <<< NEA.head
        <<< NEA.head
    )
  <*>
    ( map flattenScore
        <<< NEA.fromArray
        <<< compact
        <<< map NEA.fromArray
        <<< unrest
    )

----------------
-- dj quickcheck --

qcSamples :: NonEmptyArray Sample
qcSamples = fromMaybe (NEA.fromNonEmpty (Sample "intentionalSilenceForInternalUseOnly" :| []))
  $ NEA.fromArray
  $ map Sample
  $ O.keys urls

genSingleSample :: Gen Sample
genSingleSample = elements qcSamples

genSingleton :: forall event. Gen (Cycle (VM.Maybe (Note event)))
genSingleton =
  singleton <<< { env: { weight: 1.0, tag: VM.nothing }, val: _ } <<< VM.just <<< Note
    <<<
      { rateFoT: const 1.0
      , volumeFoT: const 1.0
      , forwardFoT: const true
      , bufferOffsetFoT: const 0.0
      , tumultFoT: const (fx $ goodbye hello)
      , sampleFoT: _
      }
    <<< VE.right <$> genSingleSample

genRest :: forall event. Gen (Cycle (VM.Maybe (Note event)))
genRest = pure $ singleton { env: { weight: 1.0, tag: VM.nothing }, val: VM.nothing }

genSingletonOrRest :: forall event. Gen (Cycle (VM.Maybe (Note event)))
genSingletonOrRest = frequency (wrap ((7.0 /\ genSingleton) :| (3.0 /\ genRest) : Nil))

genCycle :: forall event. Gen (Cycle (VM.Maybe (Note event)))
genCycle = go 0
  where
  genSameInternal nn = do
    sn <- genSingleton
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 5, 6 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 6 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4 ])
    pure $ internal { env: { weight: 1.0, tag: VM.nothing }, cycles: NEA.fromNonEmpty (sn :| A.replicate (ii - 1) sn) }
  genDiffInternal nn = do
    let gg = go (nn + 1)
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 8 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 4 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [])
    cycles <- sequence $ NEA.fromNonEmpty (gg :| A.replicate (ii - 1) gg)
    pure $ internal { env: { weight: 1.0, tag: VM.nothing }, cycles }
  genBranching nn = resize 3 (branching <<< { env: { weight: 1.0, tag: VM.nothing }, cycles: _ } <$> arrayOf1 (go (nn + 1)))
  genSimultaneous nn = resize 3 (simultaneous <<< { env: { weight: 1.0, tag: VM.nothing }, cycles: _ } <$> arrayOf1 (go (nn + 1)))
  go 0 =
    let
      zz = 0
    in
      frequency
        ( wrap
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
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (2.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (3.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go 2 =
    let
      zz = 2
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (8.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go 3 =
    let
      zz = 3
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ genSimultaneous zz)
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (10.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go _ = genSingletonOrRest

genVoice :: forall event. CycleDuration -> Gen { cycle :: Cycle (VM.Maybe (Note event)), voice :: Voice event }
genVoice cl = do
  let globals = Globals { fx: const calm }
  cycle <- genCycle
  let next = asScore false $ s2f $ c2s cycle cl
  pure $ { cycle, voice: Voice { globals, next } }

djQuickCheck :: forall event. Gen { cycle :: Cycle (VM.Maybe (Note event)), future :: TheFuture event }
djQuickCheck = do
  cl' <- arbitrary
  let cycleDuration = CycleDuration (1.0 + 3.0 * cl')
  { cycle, voice: earth } <- genVoice cycleDuration
  wind <- pure (openVoice cycleDuration)
  fire <- pure (openVoice cycleDuration)
  lambert <- pure (openVoice cycleDuration)
  hendricks <- pure (openVoice cycleDuration)
  ross <- pure (openVoice cycleDuration)
  pure $
    { cycle
    , future: TheFuture
        { earth
        , wind
        , fire
        , lambert
        , hendricks
        , ross
        , water: VM.nothing
        , heart: VM.nothing
        , title: "d j q u i c k c h e c k"
        , sounds: Object.empty
        , preload: []
        , cycleDuration
        }
    }

class S s event where
  s :: s -> CycleDuration -> Voice event

instance sProxy :: (IsSymbol sym, MiniNotation sym) => S (Proxy sym) event where
  s = s <<< reflectSymbol

instance sString :: S String event where
  s = s <<< parse

instance sCycle :: TypeEquals eventIn eventOut => S (Cycle (VM.Maybe (Note eventIn))) eventOut where
  s = s <<< rend

instance sNit :: TypeEquals eventIn eventOut => S (NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note eventIn))))) eventOut where
  s = s <<< rendNit

instance sNitFT :: TypeEquals eventIn eventOut => S (CycleDuration -> (NonEmptyArray (NonEmptyArray (NoteInTime (VM.Maybe (Note eventIn)))))) eventOut where
  s = s <<< map rendNit

instance sNift :: TypeEquals eventIn eventOut => S (NonEmptyArray (NoteInFlattenedTime (Note eventIn))) eventOut where
  s = s <<< asScore false

instance sNiftFT :: TypeEquals eventIn eventOut => S (CycleDuration -> (NonEmptyArray (NoteInFlattenedTime (Note eventIn)))) eventOut where
  s = s <<< map (asScore false)

instance sNextCycle :: TypeEquals eventIn eventOut => S (NextCycle eventIn) eventOut where
  s = s <<< (const :: NextCycle eventIn -> CycleDuration -> NextCycle eventIn)

instance sNextCycleFT :: TypeEquals eventIn eventOut => S (CycleDuration -> NextCycle eventIn) eventOut where
  s = s <<< map plainly

instance sVoice :: TypeEquals eventIn eventOut => S (Voice eventIn) eventOut where
  s = s <<< (const :: Voice eventIn -> CycleDuration -> Voice eventIn)

instance sVoiceFT :: TypeEquals eventIn eventOut => S (CycleDuration -> (Voice eventIn)) eventOut where
  s = compose (proof :: Voice eventIn -> Voice eventOut)

betwixt :: forall n. Ord n => n -> n -> n -> n
betwixt mn' mx' nn = if nn < mn then mn else if nn > mx then mx else nn
  where
  mn = min mn' mx'
  mx = max mn' mx'

oscWarp
  :: forall t386 t399 t412 t417 t423 t424 t427 t428 t434 t449 t450 t453 t454
   . Newtype t399
       ( Variant
           ( timeIs :: Record t412
           , timeIsAndWas ::
               { timeIs :: t424
               , timeWas :: t428
               | t417
               }
           , timeIsAndWasAndHadBeen ::
               { timeIs :: t450
               , timeWas :: t454
               , valHadBeen :: Number
               , valWas :: Number
               | t434
               }
           )
       )
  => Newtype t424
       { clockTime :: Number
       | t423
       }
  => Newtype t428
       { clockTime :: Number
       | t427
       }
  => Newtype t450
       { clockTime :: Number
       | t449
       }
  => Newtype t454
       { clockTime :: Number
       | t453
       }
  => { downTime :: Number
     , downWarp :: Number
     , upTime :: Number
     , upWarp :: Number
     | t386
     }
  -> t399
  -> Number
oscWarp { upTime: ut, downTime: dt, upWarp, downWarp } = unwrap >>> match
  { timeIs: \{} -> 0.0
  , timeIsAndWas: \{ timeIs, timeWas } -> upTime * ((unwrap timeIs).clockTime - (unwrap timeWas).clockTime)
  , timeIsAndWasAndHadBeen: \{ timeIs, timeWas, valWas, valHadBeen } ->
      let
        o = ((if valWas >= 1.0 then (negate downTime) else if valWas <= 0.0 then upTime else if valWas > valHadBeen then upTime else (negate downTime)) * ((unwrap timeIs).clockTime - (unwrap timeWas).clockTime)) + valWas
      in
        if o > 1.0 then 1.0 else if o < 0.0 then 0.0 else (o `pow` (exp (if valWas > valHadBeen then upWarp else downWarp)))
  }
  where
  upTime = 1.0 / (max 0.001 ut)
  downTime = 1.0 / (max 0.001 dt)

fadeTo
  :: forall t3290 t3298 t3311 t3316 t3329 t3330 t3333 t3334 t3343 t3356 t3357 t3360 t3361
   . Newtype t3298
       ( Variant
           ( timeIs :: Record t3311
           , timeIsAndWas ::
               { timeIs :: t3330
               , timeWas :: t3334
               , valWas :: Number
               | t3316
               }
           , timeIsAndWasAndHadBeen ::
               { timeIs :: t3357
               , timeWas :: t3361
               , valWas :: Number
               | t3343
               }
           )
       )
  => Newtype t3330
       { clockTime :: Number
       | t3329
       }
  => Newtype t3334
       { clockTime :: Number
       | t3333
       }
  => Newtype t3357
       { clockTime :: Number
       | t3356
       }
  => Newtype t3361
       { clockTime :: Number
       | t3360
       }
  => { duration :: Number
     , n :: Number
     | t3290
     }
  -> t3298
  -> Number
fadeTo { n: nn, duration: t } = unwrap >>> match
  { timeIs: \{} -> 0.0
  -- todo: combine
  , timeIsAndWas: \{ timeIs, timeWas, valWas } -> if valWas == nn then nn else let o = (if valWas < nn then 1.0 else (-1.0)) * duration * ((unwrap timeIs).clockTime - (unwrap timeWas).clockTime) + valWas in (if valWas < o then min else max) nn o
  , timeIsAndWasAndHadBeen: \{ timeIs, timeWas, valWas } -> if valWas == nn then nn else let o = (if valWas < nn then 1.0 else (-1.0)) * duration * ((unwrap timeIs).clockTime - (unwrap timeWas).clockTime) + valWas in (if valWas < o then min else max) nn o
  }
  where
  duration = 1.0 / (max 0.001 t)

numericTumult
  :: forall t984 t1021 t1036 t1051 t1055 t1061 t1062 t1064 t1079 t1080 t1081
   . Newtype t1021
       { param :: VM.Maybe t984
       | t1036
       }
  => Eq t1051
  => Row.Cons t1062
       { id :: t1051
       | t1055
       }
       t1064
       Instruction'
  => IsSymbol t1062
  => t984
  -> t1061 t1062
  -> t1051
  -> ( { id :: t1051
       | t1055
       }
       -> t1021
     )
  -> Tumultuous t1079 t1080 t1081
  -> t984
numericTumult dflt pxy idd fnc = fromMaybe dflt
  <<< flip A.index 0
  <<< filterMap
    ( join
        <<< map
          ( fnc
              >>> unwrap
              >>> _.param
              >>> VM.maybe Nothing Just
          )
        <<< filter (eq idd <<< _.id)
        <<< prj pxy
        <<< unwrap
    )
  <<< join
  <<< safeUntumult

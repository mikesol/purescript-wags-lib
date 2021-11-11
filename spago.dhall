{ name = "purescript-wags"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "behaviors"
  , "bifunctors"
  , "console"
  , "control"
  , "css"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "halogen"
  , "halogen-css"
  , "heterogeneous"
  , "homogeneous"
  , "identity"
  , "integers"
  , "lazy"
  , "lcg"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "record"
  , "refs"
  , "run"
  , "safe-coerce"
  , "sized-vectors"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "unfoldable"
  , "unsafe-coerce"
  , "variant"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

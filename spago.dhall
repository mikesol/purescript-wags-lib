{ name = "purescript-wags"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "behaviors"
  , "control"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "heterogeneous"
  , "identity"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "run"
  , "safe-coerce"
  , "sized-vectors"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "unfoldable"
  , "unsafe-coerce"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

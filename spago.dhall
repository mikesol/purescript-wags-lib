{ name = "purescript-wags"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "foldable-traversable"
  , "free"
  , "heterogeneous"
  , "identity"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "run"
  , "safe-coerce"
  , "sized-vectors"
  , "tailrec"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

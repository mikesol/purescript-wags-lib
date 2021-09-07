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
  , "sized-vectors"
  , "tuples"
  , "typelevel"
  , "unfoldable"
  , "unsafe-coerce"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

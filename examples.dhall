let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "random"
              , "aff"
              , "aff-promise"
              , "behaviors"
              , "console"
              , "effect"
              , "event"
              , "foreign-object"
              , "indexed-monad"
              , "parallel"
              , "profunctor"
              , "typelevel-prelude"
              ]
        }

let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "random"
              , "aff"
              , "aff-promise"
              , "behaviors"
              , "console"
              , "effect"
              , "record"
              , "event"
              , "foreign-object"
              , "indexed-monad"
              , "parallel"
              , "typelevel-prelude"
              ]
        }

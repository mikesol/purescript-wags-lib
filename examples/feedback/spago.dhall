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
              , "halogen-svg"
              , "indexed-monad"
              , "debug"
              , "parallel"
              , "typelevel-prelude"
              ]
        }

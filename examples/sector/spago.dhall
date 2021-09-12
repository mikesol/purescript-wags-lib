let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "aff"
              , "aff-promise"
              , "debug"
              , "effect"
              , "record"
              , "event"
              , "parallel"
              , "typelevel-prelude"
              ]
        }

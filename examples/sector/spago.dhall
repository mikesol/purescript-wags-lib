let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "aff"
              , "aff-promise"
              , "console"
              , "debug"
              , "effect"
              , "record"
              , "event"
              , "parallel"
              , "halogen-subscriptions"
              , "typelevel-prelude"
              ]
        }

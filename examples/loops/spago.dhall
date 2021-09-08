let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "aff"
              , "aff-promise"
              , "behaviors"
              , "effect"
              , "event"
              , "record"
              , "foreign-object"
              , "parallel"
              , "profunctor"
              ]
        }

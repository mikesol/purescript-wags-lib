let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "aff"
              , "behaviors"
              , "effect"
              , "event"
              , "string-parsers"
              ]
        }

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
              , "halogen-subscriptions"
              , "simple-json"
              , "uint"
              , "arraybuffer"
              , "strings"
              , "string-parsers"
              ]
        }

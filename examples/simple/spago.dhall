let conf = ../../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "halogen-storybook"
              , "aff"
              , "behaviors"
              , "effect"
              , "event"
              , "foreign-object"
              ]
        }

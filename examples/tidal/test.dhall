let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies = conf.dependencies # [ "debug", "spec", "const", "aff", "effect", "exceptions", "transformers" ]
        }

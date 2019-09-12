{ name =
    "propagators"
, dependencies =
    [ "aff"
    , "behaviors"
    , "console"
    , "debug"
    , "drawing"
    , "effect"
    , "enums"
    , "exists"
    , "indexed-monad"
    , "ordered-collections"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck-combinators"
    , "quickcheck-laws"
    , "spec"
    , "spec-discovery"
    , "spec-quickcheck"
    , "st"
    , "tuples"
    ]
, packages =
    ./packages.dhall
}

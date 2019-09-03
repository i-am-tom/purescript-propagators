{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "console"
    , "drawing"
    , "effect"
    , "enums"
    , "exists"
    , "ordered-collections"
    , "psci-support"
    , "spec"
    , "spec-quickcheck"
    , "st"
    , "tuples"
    ]
, packages =
    ./packages.dhall
}

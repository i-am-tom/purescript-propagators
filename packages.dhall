let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20190831/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20190831/src/packages.dhall sha256:852cd4b9e463258baf4e253e8524bcfe019124769472ca50b316fe93217c3a47

let overrides = {=}

let additions =
      { drawing =
          mkPackage
          [ "canvas"
          , "lists"
          , "math"
          , "integers"
          , "colors"
          ]
          "https://github.com/paf31/purescript-drawing.git"
          "v4.0.0"

      , quickcheck-combinators =
          mkPackage
          [ "prelude"
          , "quickcheck"
          , "typelevel"
          ]
          "https://github.com/athanclark/purescript-quickcheck-combinators"
          "v0.1.1"

--    , monad-control =
--        mkPackage
--        [ "prelude"
--        , "console"
--        , "identity"
--        , "transformers"
--        , "functors"
--        , "lists"
--        , "arrays"
--        , "freet"
--        , "aff"
--        ]
--        "https://github.com/athanclark/purescript-monad-control"
--        "v5.0.0"
      }

in  upstream // overrides // additions

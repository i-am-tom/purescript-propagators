module Data.Lattice.MaxSpec where

import Data.Lattice.Max (Max)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Max" do
  Laws.with (Proxy ∷ Proxy (Max Int))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

  Laws.with (Proxy ∷ Proxy (Max Ordering))
    [ Laws.bounded
    , Laws.boundedEnum
    , Laws.enum
    ]

module Data.Lattice.MinSpec where

import Data.Lattice.Min (Min)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Min" do
  Laws.with (Proxy ∷ Proxy (Min Int))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

  Laws.with (Proxy ∷ Proxy (Min Ordering))
    [ Laws.bounded
    , Laws.boundedEnum
    , Laws.enum
    ]

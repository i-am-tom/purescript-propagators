module Data.Lattice.AllSpec where

import Data.Lattice.All (All)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "All" do
  Laws.with (Proxy ∷ Proxy (All Boolean))
    [ Laws.bounded
    , Laws.booleanAlgebra
    , Laws.boundedEnum
    , Laws.enum
    , Laws.eq
    , Laws.heytingAlgebra
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

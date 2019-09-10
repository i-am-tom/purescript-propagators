module Data.Lattice.AnySpec where

import Data.Lattice.Any (Any)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Any" do
  Laws.with (Proxy ∷ Proxy (Any Boolean))
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

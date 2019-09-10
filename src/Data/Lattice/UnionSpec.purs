module Data.Lattice.UnionSpec where

import Data.Lattice.Union (Union)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Union" do
  Laws.with (Proxy ∷ Proxy (Union Int))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

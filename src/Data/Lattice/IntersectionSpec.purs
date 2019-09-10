module Data.Lattice.IntersectionSpec where

import Data.Lattice.Intersection (Intersection)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Intersection" do
  Laws.with (Proxy ∷ Proxy (Intersection Ordering))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

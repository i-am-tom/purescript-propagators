module Data.Semilattice.IntersectionSpec where

import Data.Semilattice.Intersection (Intersection)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Intersection" do
  Laws.with (Proxy ∷ Proxy (Intersection Ordering))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

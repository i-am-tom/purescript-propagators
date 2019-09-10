module Data.Lattice.IntervalSpec where

import Data.Lattice.Interval (Interval)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Interval" do
  Laws.with (Proxy ∷ Proxy (Interval Int))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

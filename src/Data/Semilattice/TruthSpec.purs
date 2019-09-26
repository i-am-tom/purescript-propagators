module Data.Semilattice.TruthSpec where

import Data.Semilattice.Defined (Defined)
import Data.Semilattice.Truth (Truth)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Truth" do
  Laws.with (Proxy ∷ Proxy (Truth (Defined Int)))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

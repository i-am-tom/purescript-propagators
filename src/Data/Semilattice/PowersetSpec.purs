module Data.Semilattice.PowersetSpec where

import Data.Semilattice.Defined (Defined)
import Data.Semilattice.Powerset (Powerset)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Powerset" do
  Laws.with (Proxy ∷ Proxy (Powerset (Defined Int)))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

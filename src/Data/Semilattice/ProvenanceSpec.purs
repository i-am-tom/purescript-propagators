module Data.Semilattice.ProvenanceSpec where

import Data.Semilattice.Defined (Defined)
import Data.Semilattice.Provenance (Provenance)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Provenance" do
  Laws.with (Proxy ∷ Proxy (Provenance (Defined Int)))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

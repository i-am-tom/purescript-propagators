module Data.Semilattice.DefinedSpec where

import Data.Semilattice.Defined (Defined)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Defined" do
  Laws.with (Proxy ∷ Proxy (Defined Int))
    [ Laws.eq
    , Laws.ord
    , Laws.joinSemilattice
    , Laws.monoid
    , Laws.semigroup
    ]

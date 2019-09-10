module Data.Lattice.DefinedSpec where

import Data.Lattice.Defined (Defined)
import Prelude
import Test.Laws as Laws
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Defined" do
  Laws.with (Proxy ∷ Proxy (Defined Int))
    [ Laws.eq
    , Laws.ord
    , Laws.joinSemilattice
    , Laws.monoid
    , Laws.semigroup

-- | In the Haskell world, `Num` doesn't come with all the laws that PureScript
-- asserts. We're going to ignore some laws (like multiplicative / additive
-- identities, etc), just because users shouldn't be creating non-`Known`
-- static values. Forgive us, Padre.
--
--  , Laws.commutativeRing
--  , Laws.divisionRing
--  , Laws.euclideanRing
--  , Laws.ring
--  , Laws.semiring
    ]

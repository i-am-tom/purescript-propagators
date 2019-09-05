module Data.Lattice.All where

import Data.HeytingAlgebra (tt)
import Data.Lattice (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | This semilattice effectively uses `(&&)` to combine values, and thus keeps
-- track of whether all seen values were `true`. Or, maybe more interestingly,
-- whether any computation ever _failed_.
newtype All (element ∷ Type)
  = All element

derive instance newtypeAll ∷ Newtype (All element) _
derive newtype instance eqAll   ∷ Eq   element ⇒ Eq   (All element)
derive newtype instance ordAll  ∷ Ord  element ⇒ Ord  (All element)
derive newtype instance showAll ∷ Show element ⇒ Show (All element)

instance semigroupAll
    ∷ HeytingAlgebra element ⇒ Semigroup (All element) where
  append (All this) (All that) = All (this && that)

instance monoidAll
    ∷ HeytingAlgebra element ⇒ Monoid (All element) where
  mempty = All tt

instance joinSemilatticeAll
    ∷ (HeytingAlgebra element, Ord element)
    ⇒ JoinSemilattice (All element) where
  order = compare

instance arbitraryAll ∷ Arbitrary element ⇒ Arbitrary (All element) where
  arbitrary = map All arbitrary

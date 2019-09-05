module Data.Lattice.Any where

import Data.HeytingAlgebra (ff)
import Data.Lattice (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | `Any` appends with `(||)`, and hence keeps track of whether any value were
-- ever `true`. In backtracking systems, this may be useful for determining
-- whether a solution ever exists.
newtype Any (element ∷ Type)
  = Any element

derive instance newtypeAny ∷ Newtype (Any element) _
derive newtype instance eqAny   ∷ Eq   element ⇒ Eq   (Any element)
derive newtype instance ordAny  ∷ Ord  element ⇒ Ord  (Any element)
derive newtype instance showAny ∷ Show element ⇒ Show (Any element)

instance semigroupAny
    ∷ HeytingAlgebra element ⇒ Semigroup (Any element) where
  append (Any this) (Any that) = Any (this || that)

instance monoidAny
    ∷ HeytingAlgebra element ⇒ Monoid (Any element) where
  mempty = Any ff

instance joinSemilatticeAny
    ∷ (HeytingAlgebra element, Ord element)
    ⇒ JoinSemilattice (Any element) where
  order = compare

instance arbitraryAny ∷ Arbitrary element ⇒ Arbitrary (Any element) where
  arbitrary = map Any arbitrary

module Data.Semilattice.Provenance where

import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Provenance (x ∷ Type)
  = Provenance
      { sources ∷ Set String
      , value   ∷ x
      }

derive instance eqProvenance ∷ Eq x ⇒ Eq (Provenance x)
derive instance functorProvenance ∷ Functor Provenance
derive instance genericProvenance ∷ Generic (Provenance x) _
derive instance ordProvenance ∷ Ord x ⇒ Ord (Provenance x)
derive newtype instance joinSemilatticeProvenance ∷ JoinSemilattice x ⇒ JoinSemilattice (Provenance x)
derive newtype instance monoidProvenance ∷ Monoid x ⇒ Monoid (Provenance x)
derive newtype instance semigroupProvenance ∷ Semigroup x ⇒ Semigroup (Provenance x)
derive newtype instance showProvenance ∷ Show x ⇒ Show (Provenance x)

instance foldableProvenance ∷ Foldable Provenance where
  foldl f = foldlDefault f
  foldr f = foldrDefault f

  foldMap f (Provenance { value }) = f value

instance arbitraryProvenance ∷ Arbitrary x ⇒ Arbitrary (Provenance x) where
  arbitrary = do
    sources ← map Set.fromFoldable (arbitrary ∷ _ (Array _))
    value   ← arbitrary

    pure (Provenance { sources, value })

module Data.Lattice.Union where

import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Union (x ∷ Type)
  = Union (Set x)

singleton ∷ ∀ x. Ord x ⇒ x → Union x
singleton = Union <<< Set.singleton

member ∷ ∀ x. Ord x ⇒ x → Union x → Boolean
member x (Union xs) = Set.member x xs

derive instance newtypeUnion ∷ Newtype (Union x) _
derive newtype instance eqUnion ∷ Eq x ⇒ Eq (Union x)
derive newtype instance monoidUnion ∷ Ord x ⇒ Monoid (Union x)
derive newtype instance ordUnion ∷ Ord x ⇒ Ord (Union x)
derive newtype instance semigroupUnion ∷ Ord x ⇒ Semigroup (Union x)
derive newtype instance showUnion ∷ Show x ⇒ Show (Union x)

instance joinSemilatticeUnion ∷ Ord x ⇒ JoinSemilattice (Union x) where
  order (Union this) (Union that) = compare (Set.size this) (Set.size that)

instance arbitraryUnion ∷ (Arbitrary x, Ord x) ⇒ Arbitrary (Union x) where
  arbitrary = do
    xs ∷ Array x ← arbitrary
    pure (Union (Set.fromFoldable xs))

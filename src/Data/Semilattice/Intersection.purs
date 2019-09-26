module Data.Semilattice.Intersection where

import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Intersection (x ∷ Type)
  = Intersection (Set x)

derive instance newtypeIntersection ∷ Newtype (Intersection x) _
derive newtype instance eqIntersection ∷ Eq x ⇒ Eq (Intersection x)
derive newtype instance ordIntersection ∷ Ord x ⇒ Ord (Intersection x)
derive newtype instance showIntersection ∷ Show x ⇒ Show (Intersection x)

instance semigroupIntersection ∷ Ord x ⇒ Semigroup (Intersection x) where
  append (Intersection this) (Intersection that)
    = Intersection (Set.intersection this that)

instance monoidIntersection ∷ BoundedEnum x ⇒ Monoid (Intersection x) where
  mempty = Intersection (Set.fromFoldable (upFromIncluding bottom ∷ Array _))

instance joinSemilatticeIntersection
  ∷ BoundedEnum x
  ⇒ JoinSemilattice (Intersection x)

instance arbitraryIntersection
    ∷ (Arbitrary x, Ord x)
    ⇒ Arbitrary (Intersection x) where
  arbitrary = do
    xs ∷ Array x ← arbitrary
    pure (Intersection (Set.fromFoldable xs))

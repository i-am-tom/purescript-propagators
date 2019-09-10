module Data.Lattice.Intersection where

import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Intersection (element ∷ Type)
  = Intersection (Set element)

derive         instance newtypeIntersection ∷ Newtype (Intersection element) _
derive newtype instance eqIntersection      ∷ Eq element   ⇒ Eq (Intersection element)
derive newtype instance ordIntersection     ∷ Ord element  ⇒ Ord (Intersection element)
derive newtype instance showIntersection    ∷ Show element ⇒ Show (Intersection element)

instance semigroupIntersection
    ∷ Ord element ⇒ Semigroup (Intersection element) where
  append (Intersection this) (Intersection that)
    = Intersection (Set.intersection this that)

instance monoidIntersection
    ∷ BoundedEnum element ⇒ Monoid (Intersection element) where
  mempty = Intersection
    $ (Set.fromFoldable (upFromIncluding bottom ∷ Array element))

instance joinSemilatticeIntersection
    ∷ BoundedEnum element ⇒ JoinSemilattice (Intersection element) where
  order (Intersection this) (Intersection that)
    = compare (Set.size that) (Set.size this)

instance arbitraryIntersection
    ∷ (Arbitrary element, Ord element)
    ⇒ Arbitrary (Intersection element) where
  arbitrary = do
    elements ∷ Array element ← arbitrary
    pure (Intersection (Set.fromFoldable elements))

module Data.Lattice.Intersection where

import Data.Enum (class BoundedEnum, upFrom)
import Data.Lattice (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Let's imagine we're in a real-life game of Cluedo. There are two ways to
-- solve the mystery (and the other one is described in `Union`), but we'll
-- focus one one of them for now:
--
-- We can start with the set of all possible answers, and rule out
-- impossibilities as we discover them. With the `Intersection` semilattice,
-- the set of all possible values (or, in this case, crimes) is the "bottom" of
-- the lattice, and we refine our set of possibilities as we ascend.
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
  mempty = Intersection (Set.fromFoldable (upFrom bottom ∷ Array element))

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

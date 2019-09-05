module Data.Lattice.Union where

import Data.Lattice (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Let's imagine we're in a real-life game of Cluedo. There are two ways to
-- solve the mystery (and the other one is described in `Intersection`), but
-- we'll focus one one of them for now:
--
-- We can start by saying we know _nothing_, and collect facts as they are
-- established. The bottom of the `Union` lattice is an empty set: we have
-- nothing that we know. As we ascend the lattice, this is analogous to
-- "learning more facts".
newtype Union (element ∷ Type)
  = Union (Set element)

derive         instance newtypeUnion   ∷ Newtype (Union element) _
derive newtype instance eqUnion        ∷ Eq element   ⇒ Eq (Union element)
derive newtype instance ordUnion       ∷ Ord element  ⇒ Ord (Union element)
derive newtype instance showUnion      ∷ Show element ⇒ Show (Union element)
derive newtype instance semigroupUnion ∷ Ord element  ⇒ Semigroup (Union element)
derive newtype instance monoidUnion    ∷ Ord element  ⇒ Monoid (Union element)

instance joinSemilatticeUnion
    ∷ Ord element ⇒ JoinSemilattice (Union element) where
  order (Union this) (Union that)
    = compare (Set.size this) (Set.size that)

instance arbitraryUnion
    ∷ ( Arbitrary element
      , Ord element
      )
    ⇒ Arbitrary (Union element) where
  arbitrary = do
    elements ∷ Array element ← arbitrary
    pure (Union (Set.fromFoldable elements))

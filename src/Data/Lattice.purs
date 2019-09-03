module Data.Lattice where

import Data.Enum (class BoundedEnum, upFrom)
import Data.Function (on)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Newtype (class Newtype)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Prelude
import Test.QuickCheck (class Arbitrary)

-- | A join semilattice is a monoid whose `mappend` is both idempotent and
-- commutative. We often visualise this with a Hasse diagram, but don't worry
-- too much about this if an image search frightens you. It should become
-- clearer as we see examples.
--
-- For now, let's use a basic intuition: `bottom` (`mempty`) means "I don't
-- know anything". The more things we `mappend`, the further from `bottom` we
-- move, and "the more we know".
class Monoid element ⇐ JoinSemilattice (element ∷ Type) where

  -- | In the language of lattices, `mempty` is often referred to as `bottom`.
  -- Hence, in our domain, `order` refers to a comparison of the "distances
  -- from `bottom`. In other words, `order this that = GT` if `this` is a "more
  -- refined" value than `that`.
  order ∷ element → element → Ordering

-- | The simplest case of a semilattice is the unit semilattice.
instance joinSemilatticeUnit ∷ JoinSemilattice Unit where
  order _ _ = EQ

-- | Semilattices are preserved over products, which is something that's worth
-- some extra emphasis: when Edward Kmett says, "a propagator is a monotone
-- function between join semilattices", this instance is what means that a
-- propagator can relate _any number of semilattices_.
instance joinSemilatticeTuple
    ∷ ( JoinSemilattice left
      , JoinSemilattice right
      )
    ⇒ JoinSemilattice (left /\ right) where
  order (thisx /\ thisy) (thatx /\ thaty)
    = order thisx thatx <> order thisy thaty

-- | The default behaviour of sets as monoids is that `mappend` behaves as a
-- union. Thus, a value tells us /more/ than another if it has more elements.
instance joinSemilatticeSet
    ∷ Ord element
    ⇒ JoinSemilattice (Set element) where
  order = compare `on` Set.size

-- | Set intersection works pretty much the same way as the above. We need a
-- newtype for this, though, as union is the "default" `Set` append behaviour.
newtype Intersect (element ∷ Type) = Intersect (Set element)

derive         instance newtypeIntersect ∷ Newtype (Intersect element) _
derive newtype instance eqIntersect      ∷ Eq element ⇒ Eq (Intersect element)
derive newtype instance ordIntersect     ∷ Ord element ⇒ Ord (Intersect element)

instance semigroupIntersect
    ∷ Ord element
    ⇒ Semigroup (Intersect element) where
  append (Intersect this) (Intersect that)
    = Intersect (Set.intersection this that)

instance monoidIntersect
    ∷ BoundedEnum element
    ⇒ Monoid (Intersect element) where
  mempty = Intersect (Set.fromFoldable (upFrom bottom ∷ Array element))

instance joinSemilatticeIntersect
    ∷ BoundedEnum element
    ⇒ JoinSemilattice (Intersect element) where
  order = flip compare `on` \(Intersect set) → Set.size set

instance joinSemilatticeMax
    ∷ Bounded element
    ⇒ JoinSemilattice (Max element) where
  order = compare

instance joinSemilatticeMin
    ∷ Bounded element
    ⇒ JoinSemilattice (Min element) where
  order = flip compare

instance joinSemilatticeConj
    ∷ ( HeytingAlgebra element
      , Ord element
      )
    ⇒ JoinSemilattice (Conj element) where
  order = flip compare

instance joinSemilatticeDisj
    ∷ ( HeytingAlgebra element
      , Ord element
      )
    ⇒ JoinSemilattice (Disj element) where
  order = compare

-- | If `this` _implies_ `that`, we mean appending `this` to `that` will not
-- change `this`. In our domain, what we're really saying is that `that` won't
-- tell us anything that `this` doesn't already know.
--
-- There's one law here to bear in mind: if `this` implies `that`, then it
-- should be the case that `order this that /= LT`. We can hence think of
-- ordering as a formalism of "who's furthest from the bottom of the lattice".
implies
  ∷ ∀ element
  . JoinSemilattice element
  ⇒ element
  → element
  → Boolean

implies this that
  = order this (this <> that) /= LT

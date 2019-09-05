module Data.Lattice where

import Data.Monoid (class MonoidRecord)
import Data.Symbol (class IsSymbol, SProxy (..))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record as Record
import Type.Data.RowList (RLProxy (..))

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

class JoinSemilatticeRowlist (row ∷ # Type) (list ∷ RowList) where
  orderRL ∷ RLProxy list → Record row → Record row → Ordering

instance joinSemilatticeNil
    ∷ JoinSemilatticeRowlist row Nil where
  orderRL _ _ _ = EQ

instance joinSemilatticeCons
    ∷ ( IsSymbol key
      , JoinSemilattice element
      , JoinSemilatticeRowlist row tail
      , Row.Cons key element other row
      )
    ⇒ JoinSemilatticeRowlist row (Cons key element tail) where
  orderRL _ this that
    = order (Record.get label this ∷ element) (Record.get label that)
        <> orderRL (RLProxy ∷ RLProxy tail) this that
    where
      label ∷ SProxy key
      label = SProxy

-- | Any record, being isomorphic to a product, is a semilattice if all its
-- component parts are also semilattices. This lets us hold multiple pieces of
-- related information within a cell.
instance joinSemilatticeRecord
    ∷ ( RowToList row list
      , MonoidRecord list row row
      , JoinSemilatticeRowlist row list
      )
    ⇒ JoinSemilattice (Record row) where
  order = orderRL (RLProxy ∷ RLProxy list)

-- | If `this` _implies_ `that`, we mean appending `this` to `that` will not
-- change `this`. In our domain, what we're really saying is that `that` won't
-- tell us anything that `this` doesn't already know.
--
-- There's one law here to bear in mind: if `this` implies `that`, then it
-- should be the case that `order this that /= LT`. We can hence think of
-- ordering as a formalism of "who's furthest from the bottom of the lattice".
implies ∷ ∀ element. JoinSemilattice element ⇒ element → element → Boolean
implies this that = order this (this <> that) /= LT

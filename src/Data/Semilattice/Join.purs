module Data.Semilattice.Join where

import Data.Monoid (class MonoidRecord)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)

-- A join semilattice is an idempotent, commutative monoid. In other words, the
-- following laws apply to any implementing type:
--
-- * Associativity: `(a <> b) <> c === a <> (b <> c)`
-- * Identity: `mempty <> a === a`
-- * Idempotence: `a <> a === a`
-- * Commutativity: `a <> b === b <> a`
--
-- In our domain, these laws allow us to work independent of evaluation
-- strategy, and facilitate parallelism across distributed systems. If we don't
-- care about the _order_ in which answers are calculated, and redundant
-- information makes no difference, then we cacn choose any method of
-- parallelisation and guarantee the same result.
--
-- Idempotence gives us a particularly interesting property: if `a <> b === a`,
-- we say that `a` _implies_ `b`. In other words, knowing `b` hasn't told us
-- anything that knowing `a` didn't already tell us. With this property, we can
-- establish an "order" among values in a semilattice. Specifically, `mempty`
-- sits at the bottom (as _every_ value "implies" `mempty`), and "total" or
-- "contradictory" knowledge sits at the top (if we can prove a contradiction,
-- we can prove anything, right?)
--
-- Within the domain of propagators, we require that every function is
-- "monotonic". The formal definition of a monotonic function `f` is that, for
-- some ordering `⇐`, `x ⇐ y` implies `f x ⇐ f y`. We're lucky in our case:
-- idempotence means that no write can "remove" knowledge (at least, not in the
-- semilattice implication sense of the word), and thus functions are
-- necessarily monotonic.
class Monoid element ⇐ JoinSemilattice (element ∷ Type)

instance joinSemilatticeAll ∷ HeytingAlgebra x ⇒ JoinSemilattice (Conj x)
instance joinSemilatticeAny ∷ HeytingAlgebra x ⇒ JoinSemilattice (Disj x)

instance joinSemilatticeMax ∷ Bounded x ⇒ JoinSemilattice (Max x)
instance joinSemilatticeMin ∷ Bounded x ⇒ JoinSemilattice (Min x)

instance joinSemilatticeAdditive       ∷ Semiring x ⇒ JoinSemilattice (Additive x)
instance joinSemilatticeMultiplicative ∷ Semiring x ⇒ JoinSemilattice (Multiplicative x)

instance joinSemilatticeSet ∷ Ord x ⇒ JoinSemilattice (Set x)
instance joinSemilatticeUnit ∷ JoinSemilattice  Unit

instance joinSemilatticePair
    ∷ (JoinSemilattice left, JoinSemilattice right)
    ⇒ JoinSemilattice (left /\ right)

class JoinSemilatticeRowlist (list ∷ RowList)

instance joinSemilatticeNil
  ∷ JoinSemilatticeRowlist Nil

instance joinSemilatticeCons
  ∷ ( JoinSemilattice head
    , JoinSemilatticeRowlist tail
    )
  ⇒ JoinSemilatticeRowlist (Cons key head tail)

instance joinSemilatticeRecord
    ∷ ( RowToList row list
      , MonoidRecord list row row
      , JoinSemilatticeRowlist list
      )
    ⇒ JoinSemilattice (Record row)

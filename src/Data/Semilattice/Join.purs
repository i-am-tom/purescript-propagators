module Data.Semilattice.Join
  ( class JoinSemilattice
  , class JoinSemilatticeRecord

  , implies
  , order
  ) where

import Data.Monoid (class MonoidRecord)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))
import Prelude
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)

class Monoid x ⇐ JoinSemilattice (x ∷ Type)

implies ∷ ∀ x. JoinSemilattice x ⇒ Eq x ⇒ x → x → Boolean
implies this that = order this that == GT

order ∷ ∀ x. JoinSemilattice x ⇒ Eq x ⇒ x → x → Ordering
order this that = case this <> that of
  these | this == these → GT
        | that == these → LT
        | otherwise     → EQ

instance joinSemilatticeAll
  ∷ HeytingAlgebra x
  ⇒ JoinSemilattice (Conj x)

instance joinSemilatticeAny
  ∷ HeytingAlgebra x
  ⇒ JoinSemilattice (Disj x)

instance joinSemilatticeMax
  ∷ Bounded x
  ⇒ JoinSemilattice (Max x)

instance joinSemilatticeMin
  ∷ Bounded x
  ⇒ JoinSemilattice (Min x)

instance joinSemilatticeSet
  ∷ Ord x
  ⇒ JoinSemilattice (Set x)

instance joinSemilatticeUnit
  ∷ JoinSemilattice Unit

instance joinSemilatticePair
  ∷ (JoinSemilattice l, JoinSemilattice r)
  ⇒ JoinSemilattice (l /\ r)

instance joinSemilatticeFunction
  ∷ JoinSemilattice o
  ⇒ JoinSemilattice (i → o)

instance joinSemilatticeRecord
  ∷ ( JoinSemilatticeRecord list
    , MonoidRecord list row row
    , RowToList row list
    )
  ⇒ JoinSemilattice (Record row)

class JoinSemilatticeRecord (list ∷ RowList)
instance consJoinSemilatticeRecord ∷ (JoinSemilattice head, JoinSemilatticeRecord tail) ⇒ JoinSemilatticeRecord (Cons k head tail)
instance nilJoinSemilatticeRecord ∷ JoinSemilatticeRecord Nil

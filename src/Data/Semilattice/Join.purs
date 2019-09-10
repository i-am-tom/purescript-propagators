module Data.Semilattice.Join where

import Prelude

class Monoid element ⇐ JoinSemilattice (element ∷ Type) where
  order ∷ element → element → Ordering

implies ∷ ∀ element. JoinSemilattice element ⇒ element → element → Boolean
implies this that = order this (this <> that) /= LT

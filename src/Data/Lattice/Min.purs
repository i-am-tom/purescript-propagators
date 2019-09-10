module Data.Lattice.Min
  ( Min (..)
  ) where

import Data.Enum (class BoundedEnum, class Enum)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Min (element ∷ Type) = Min element

derive         instance newtypeMin     ∷ Newtype (Min element) _
derive newtype instance eqMin          ∷ Eq      element     ⇒ Eq          (Min element)
derive newtype instance ordMin         ∷ Ord     element     ⇒ Ord         (Min element)
derive newtype instance showMin        ∷ Show    element     ⇒ Show        (Min element)
derive newtype instance enumMin        ∷ Enum    element     ⇒ Enum        (Min element)
derive newtype instance boundedMin     ∷ Bounded element     ⇒ Bounded     (Min element)
derive newtype instance boundedEnumMin ∷ BoundedEnum element ⇒ BoundedEnum (Min element)

instance semigroupMin
    ∷ Ord element ⇒ Semigroup (Min element) where
  append (Min this) (Min that) = Min (min this that)

instance monoidMin
    ∷ Bounded element ⇒ Monoid (Min element) where
  mempty = Min top

instance joinSemilatticeMin
    ∷ Bounded element ⇒ JoinSemilattice (Min element) where
  order = flip compare

instance arbitraryMin
    ∷ Arbitrary element ⇒ Arbitrary (Min element) where
  arbitrary = map Min arbitrary

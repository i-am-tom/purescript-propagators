module Data.Lattice.All where

import Data.Enum (class BoundedEnum, class Enum)
import Data.HeytingAlgebra (tt)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype All (element ∷ Type)
  = All element

derive instance newtypeAll ∷ Newtype (All x) _
derive newtype instance boundedAll ∷ Bounded x ⇒ Bounded (All x)
derive newtype instance boundedEnumAll ∷ BoundedEnum x ⇒ BoundedEnum (All x)
derive newtype instance enumAll ∷ Enum x ⇒ Enum (All x)
derive newtype instance eqAll ∷ Eq x ⇒ Eq (All x)
derive newtype instance ordAll ∷ Ord x ⇒ Ord (All x)
derive newtype instance showAll ∷ Show x ⇒ Show (All x)

derive newtype instance booleanAlgebraAll ∷ BooleanAlgebra x ⇒ BooleanAlgebra (All x)
derive newtype instance heytingAlgebraAll ∷ HeytingAlgebra x ⇒ HeytingAlgebra (All x)

instance semigroupAll ∷ HeytingAlgebra x ⇒ Semigroup (All x) where
  append (All this) (All that) = All (this && that)

instance monoidAll ∷ HeytingAlgebra x ⇒ Monoid (All x) where
  mempty = All tt

instance joinSemilatticeAll ∷ (HeytingAlgebra x, Ord x) ⇒ JoinSemilattice (All x) where
  order = compare

instance arbitraryAll ∷ Arbitrary element ⇒ Arbitrary (All element) where
  arbitrary = map All arbitrary

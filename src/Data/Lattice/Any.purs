module Data.Lattice.Any where

import Data.Enum (class BoundedEnum, class Enum)
import Data.HeytingAlgebra (ff)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Any (element ∷ Type)
  = Any element

derive instance newtypeAny ∷ Newtype (Any element) _
derive newtype instance boundedAny ∷ Bounded x ⇒ Bounded (Any x)
derive newtype instance boundedEnumAny ∷ BoundedEnum x ⇒ BoundedEnum (Any x)
derive newtype instance enumAny ∷ Enum x ⇒ Enum (Any x)
derive newtype instance eqAny ∷ Eq x ⇒ Eq   (Any x)
derive newtype instance ordAny ∷ Ord x ⇒ Ord  (Any x)
derive newtype instance showAny ∷ Show x ⇒ Show (Any x)

derive newtype instance booleanAlgebraAny ∷ BooleanAlgebra x ⇒ BooleanAlgebra (Any x)
derive newtype instance heytingAlgebraAny ∷ HeytingAlgebra x ⇒ HeytingAlgebra (Any x)

instance semigroupAny ∷ HeytingAlgebra x ⇒ Semigroup (Any x) where
  append (Any this) (Any that) = Any (this || that)

instance monoidAny ∷ HeytingAlgebra x ⇒ Monoid (Any x) where
  mempty = Any ff

instance joinSemilatticeAny ∷ (HeytingAlgebra x, Ord x) ⇒ JoinSemilattice (Any x) where
  order = compare

instance arbitraryAny ∷ Arbitrary x ⇒ Arbitrary (Any x) where
  arbitrary = map Any arbitrary

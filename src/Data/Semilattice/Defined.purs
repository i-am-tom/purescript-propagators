module Data.Semilattice.Defined where

import Data.Generic.Rep (class Generic, from)
import Data.Generic.Rep.Show (genericShow)
import Data.Semilattice.Join (class JoinSemilattice)
import Prelude
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (class Coarbitrary, genericArbitrary, coarbitrary)

data Defined (x ∷ Type)
  = Unknown
  | Known x
  | Contradiction

derive instance eqDefined ∷ Eq x ⇒ Eq (Defined x)
derive instance functorDefined ∷ Functor Defined
derive instance genericDefined ∷ Generic (Defined x) _
derive instance ordDefined ∷ Ord x ⇒ Ord (Defined x)

instance showDefined ∷ Show x ⇒ Show (Defined x) where
  show = genericShow

instance semigroupDefined
    ∷ Eq x
    ⇒ Semigroup (Defined x) where
  append = case _, _ of
    this, Unknown → this
    Unknown, that → that

    Known x, Known y
      | x == y    → Known x
      | otherwise → Contradiction

    Contradiction, _ → Contradiction
    _, Contradiction → Contradiction

binary ∷ ∀ x. (x → x → x) → Defined x → Defined x → Defined x
binary f = case _, _ of
  Unknown,       x             → x
  x,             Unknown       → x
  Known x,       Known y       → Known (f x y)
  Contradiction, _             → Contradiction
  _,             Contradiction → Contradiction

instance monoidDefined
    ∷ Eq x
    ⇒ Monoid (Defined x) where
  mempty = Unknown

instance joinSemilatticeDefined
  ∷ Eq x
  ⇒ JoinSemilattice (Defined x)

instance arbitraryDefined
    ∷ Arbitrary x
    ⇒ Arbitrary (Defined x) where
  arbitrary = genericArbitrary

instance coarbitraryDefined
    ∷ Coarbitrary x
    ⇒ Coarbitrary (Defined x) where
  coarbitrary x = coarbitrary (from x)

instance commutativeRingDefined
  ∷ CommutativeRing x
  ⇒ CommutativeRing (Defined x)

instance divisionRingDefined
    ∷ DivisionRing x
    ⇒ DivisionRing (Defined x) where
  recip = map recip

instance euclideanRingDefined
    ∷ EuclideanRing x
    ⇒ EuclideanRing (Defined x) where
  degree _ = 1
  div = binary div
  mod = binary mod

instance ringDefined
    ∷ Ring x
    ⇒ Ring (Defined x) where
  sub = binary sub

instance semiringDefined
    ∷ Semiring x
    ⇒ Semiring (Defined x) where
  add  = binary add
  mul  = binary mul
  one  = Known one
  zero = Known zero

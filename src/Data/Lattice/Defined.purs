module Data.Lattice.Defined where

import Data.Lattice (class JoinSemilattice)
import Data.NonEmpty ((:|))
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

-- | The `Defined` type describes a value that we either know, don't know, or
-- have conflicting information about.
--
-- The idea here is that, for simple computations, we simply want to know a
-- value, and check whether all reports of this value be consistent. To that
-- end, we have a very simple semilattice for doing exactly this. We can use
-- `Known` to assert a value, and `Contradiction` keeps track of any
-- inconsistences in the network.
data Defined (element ∷ Type)
  = Unknown
  | Known element
  | Contradiction (Set element)

instance eqDefined ∷ Eq element ⇒ Eq (Defined element) where
  eq (Unknown        ) (Unknown        ) = true
  eq (Known x        ) (Known y        ) = x == y
  eq (Contradiction _) (Contradiction _) = true
  eq  _                 _                = false

instance showDefined ∷ Show element ⇒ Show (Defined element) where
  show = case _ of
    Unknown          → "Unknown"
    Known x          → "Known "         <> show x
    Contradiction xs → "Contradiction " <> show xs

instance arbitraryDefined
    ∷ ( Arbitrary element
      , Ord element
      )
    ⇒ Arbitrary (Defined element) where
  arbitrary = do
    content                  ← arbitrary
    failures ∷ Array element ← arbitrary

    elements $ Unknown :|
      [ Known content
      , Contradiction (Set.fromFoldable failures)
      ]

instance semigroupDefined
    ∷ Ord element
    ⇒ Semigroup (Defined element) where
  append = case _, _ of
    Unknown, that → that
    this, Unknown → this

    Known this, Known that
      | this == that → Known this
      | otherwise    → Contradiction (Set.fromFoldable [this, that])

    Contradiction this, Contradiction that →
      Contradiction (this <> that)

    Contradiction this, _ →
      Contradiction this

    _, Contradiction that →
      Contradiction that

instance monoidDefined
    ∷ Ord element ⇒ Monoid (Defined element) where
  mempty = Unknown

instance joinSemilatticeDefined
    ∷ Ord element ⇒ JoinSemilattice (Defined element) where
  order = case _, _ of
    Unknown,         Unknown → EQ
    Known _,         Known _ → EQ
    Contradiction _, Contradiction _ → EQ

    Unknown, _ → LT
    _, Unknown → GT

    Contradiction _, _ → GT
    _, Contradiction _ → LT

binary ∷ ∀ a. Ord a ⇒ (a → a → a) → Defined a → Defined a → Defined a
binary f = case _, _ of
  Known this,      Known that      → Known (f this that)

  Contradiction x, Contradiction y → Contradiction (x <> y)
  Contradiction x, _               → Contradiction  x
  _,               Contradiction y → Contradiction       y

  Unknown,         _               → Unknown
  _,               Unknown         → Unknown

instance commutativeRingDefined
  ∷ ( CommutativeRing element
    , Ord element
    )
  ⇒ CommutativeRing (Defined element)

instance euclideanRingDefined
    ∷ ( EuclideanRing element
      , Ord element
      )
    ⇒ EuclideanRing (Defined element) where
  degree _ = 1
  div      = binary div
  mod      = binary mod

instance ringDefined
    ∷ ( Ord element
      , Ring element
      )
    ⇒ Ring (Defined element) where
  sub = binary sub

instance semiringDefined
    ∷ ( Ord element
      , Semiring element
      )
    ⇒ Semiring (Defined element) where
  add  = binary add
  mul  = binary mul
  one  = Known one
  zero = Known zero

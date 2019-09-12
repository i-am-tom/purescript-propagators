module Data.Semilattice.Defined where

import Data.Semilattice.Join (class JoinSemilattice)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

-- | One of the neatest semilattices: this cell contains a value we either "know
-- nothing about", "definitely 100% know", or "have conflicting reports". This
-- is also the order of ascension through the semilattice: knowing nothing,
-- knowing something, knowing too many things.
data Defined (element ∷ Type)
  = Unknown
  | Known element
  | Contradiction (Set element)

instance eqDefined ∷ Eq element ⇒ Eq (Defined element) where
  eq (Unknown        ) (Unknown        ) = true
  eq (Known x        ) (Known y        ) = x == y
  eq (Contradiction _) (Contradiction _) = true
  eq  _                 _                = false

instance ordDefined ∷ Ord element ⇒ Ord (Defined element) where
  compare (Unknown        ) (Unknown        ) = EQ
  compare (Unknown        )  _                = LT
  compare _                 (Unknown        ) = GT
  compare (Known x        ) (Known y        ) = compare x y
  compare (Contradiction _) (Contradiction _) = EQ
  compare (Contradiction _)  _                = GT
  compare _                 (Contradiction _) = LT

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
    content                           ← arbitrary
    failures ∷ NonEmpty Array element ← arbitrary

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
    ∷ Ord element ⇒ JoinSemilattice (Defined element)

binary ∷ ∀ a. Ord a ⇒ (a → a → a) → Defined a → Defined a → Defined a
binary f = case _, _ of
  Known this,      Known that      → Known (f this that)

  Contradiction x, Contradiction y → Contradiction (x <> y)
  Contradiction x, _               → Contradiction  x
  _,               Contradiction y → Contradiction       y

  Unknown,         that            → that
  this,            Unknown         → this

instance commutativeRingDefined
  ∷ ( CommutativeRing element
    , Ord element
    )
  ⇒ CommutativeRing (Defined element)

instance divisionRingDefined
    ∷ ( DivisionRing element
      , Ord element
      )
    ⇒ DivisionRing (Defined element) where
  recip = case _ of
    Known x         → Known (recip x)
    Unknown         → Unknown
    Contradiction x → Contradiction x

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

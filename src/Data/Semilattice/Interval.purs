module Data.Semilattice.Interval where

import Data.Newtype (unwrap)
import Data.Ord.Max (Max (..))
import Data.Ord.Min (Min (..))
import Data.Semilattice.Join (class JoinSemilattice)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- The Interval type gives us a way of expressing precision; it might be easier
-- to think of it as being "the answer is within this range", in order to
-- preserve the value "description" analogy from `Data.Semilattice.Join`.
--
-- Appending here works by intersection: we take the highest lower bound and the
-- lowest upper bound to "refine" our estimation. Should we ever encounter a
-- contradiction, the `Impossible` value denotes an interval with no
-- inhabitants.
data Interval (element ∷ Type)
  = Interval
      { lower ∷ Max element
      , upper ∷ Min element
      }

  | Impossible

derive instance eqInterval  ∷ Eq element ⇒ Eq (Interval element)
derive instance ordInterval ∷ Ord element ⇒ Ord (Interval element)

instance showInterval ∷ Show element ⇒ Show (Interval element) where
  show = case _ of
    Interval { lower, upper } → "[ " <> show lower <> " .. " <> show upper <> " ]"
    Impossible                → "Impossible"

instance semigroupInterval
    ∷ Ord element ⇒ Semigroup (Interval element) where
  append = case _, _ of
    Impossible, _ → Impossible
    _, Impossible → Impossible

    Interval this, Interval that → do
      let joined = this <> that

      if unwrap joined.lower >= unwrap joined.upper
        then Impossible
        else Interval joined

instance monoidInterval
    ∷ Bounded element ⇒ Monoid (Interval element) where
  mempty = Interval { lower: Max bottom, upper: Min top }

instance joinSemilatticeInterval
    ∷ (Bounded element, Ring element)
    ⇒ JoinSemilattice (Interval element)

instance arbitraryInterval
    ∷ (Arbitrary element, Ord element) ⇒ Arbitrary (Interval element) where
  arbitrary = do
    this ← arbitrary
    that ← arbitrary

    pure case compare this that of
      LT → Interval { lower: Max this, upper: Min that }
      EQ → Impossible
      GT → Interval { lower: Max that, upper: Min this }

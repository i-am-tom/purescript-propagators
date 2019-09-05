module Data.Lattice.Interval where

import Prelude
import Data.Lattice (class JoinSemilattice)
import Data.Lattice.Max (Max (..))
import Data.Lattice.Min (Min (..))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | An interval is a pair of an upper and a lower bound. We combine intervals
-- by picking the greatest lower bound and the smallest upper bound, thus
-- effectively "tightening" the interval. Should the lower bound ever be
-- smaller than the upper bound, we consider this "impossible" (effectively a
-- contradiction).
--
-- Intervals are useful when calculating a value from multiple sources with
-- differing error margins, as well as modelling optimising calculations.
data Interval (element ∷ Type)
  = Interval
      { lower ∷ Max element
      , upper ∷ Min element
      }

  | Impossible

derive instance eqInterval  ∷ Eq element ⇒ Eq (Interval element)
derive instance ordInterval ∷ Ord element ⇒ Ord (Interval element)

instance semigroupInterval
    ∷ Ord element ⇒ Semigroup (Interval element) where
  append = case _, _ of
    Impossible, _ → Impossible
    _, Impossible → Impossible

    Interval this, Interval that → do
      let joined@{ lower: Max lower, upper: Min upper }
            = { lower: this.lower <> that.lower
              , upper: this.upper <> that.upper
              }

      if lower > upper
        then Impossible
        else Interval joined

instance monoidInterval
    ∷ Bounded element ⇒ Monoid (Interval element) where
  mempty = Interval { lower: Max bottom, upper: Min top }

instance joinSemilatticeInterval
    ∷ (Bounded element, Ring element)
    ⇒ JoinSemilattice (Interval element) where
  order = case _, _ of
    Impossible, Impossible → EQ
    Impossible, _          → LT
    _,          Impossible → GT

    Interval { lower: Max ax, upper: Min ay },
        Interval { lower: Max bx, upper: Min by } ->
      compare (by - bx) (ay - ax)

instance arbitraryInterval
    ∷ (Arbitrary element, Ord element) ⇒ Arbitrary (Interval element) where
  arbitrary = do
    this ← arbitrary
    that ← arbitrary

    pure case compare this that of
      LT → Interval { lower: Max this, upper: Min that }
      _  → Interval { lower: Max that, upper: Min this }

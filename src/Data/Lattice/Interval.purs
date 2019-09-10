module Data.Lattice.Interval where

import Prelude
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Lattice.Max (Max (..))
import Data.Lattice.Min (Min (..))
import Test.QuickCheck (class Arbitrary, arbitrary)

data Interval (element ∷ Type)
  = Interval
      { lower ∷ Max element
      , upper ∷ Min element
      }

  | Impossible

derive instance eqInterval  ∷ Eq element ⇒ Eq (Interval element)
derive instance ordInterval ∷ Ord element ⇒ Ord (Interval element)

instance showInterval
    ∷ Show element ⇒ Show (Interval element) where
  show = case _ of
    Interval { lower, upper } → show lower <> " .. " <> show upper
    Impossible                → "Impossible"

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

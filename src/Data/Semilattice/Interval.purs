module Data.Semilattice.Interval where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Ord.Max (Max (..))
import Data.Ord.Min (Min (..))
import Data.Semilattice.Join (class JoinSemilattice)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

data Interval (x ∷ Type)
  = Interval
      { lower ∷ Max x
      , upper ∷ Min x
      }

  | Contradiction

derive instance eqInterval      ∷ Eq x  ⇒ Eq      (Interval x)
derive instance ordInterval     ∷ Ord x ⇒ Ord     (Interval x)
derive instance genericInterval ∷         Generic (Interval x) _

instance showInterval ∷ Show x ⇒ Show (Interval x) where
  show = genericShow

instance semigroupInterval ∷ Ord x ⇒ Semigroup (Interval x) where
  append = case _, _ of
    Contradiction, _ → Contradiction
    _, Contradiction → Contradiction

    Interval this, Interval that → do
      let joined@{ lower: Max lower, upper: Min upper } = this <> that
      if lower > upper then Contradiction else Interval joined

instance monoidInterval ∷ Bounded x ⇒ Monoid (Interval x) where
  mempty = Interval { lower: Max bottom, upper: Min top }

instance jslInterval ∷ (Bounded x, Ring x) ⇒ JoinSemilattice (Interval x)

instance arbitraryInterval ∷ (Arbitrary x, Ord x) ⇒ Arbitrary (Interval x) where
  arbitrary = do
    this ← arbitrary
    that ← arbitrary

    pure case compare this that of
      LT → Interval { lower: Max this, upper: Min that }
      EQ → Contradiction
      GT → Interval { lower: Max that, upper: Min this }

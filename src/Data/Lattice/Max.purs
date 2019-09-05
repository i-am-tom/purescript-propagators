module Data.Lattice.Max
  ( Max (..)
  ) where

import Data.Lattice (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | The `Max` lattice keeps track of the "largest" value it has seen so far.
-- This might make sense, for example, if you're trying to pick inputs for a
-- maximal output.
newtype Max (element ∷ Type) = Max element

derive         instance newtypeMax ∷ Newtype (Max element) _
derive newtype instance eqMax      ∷ Eq element   ⇒ Eq   (Max element)
derive newtype instance ordMax     ∷ Ord element  ⇒ Ord  (Max element)
derive newtype instance showMax    ∷ Show element ⇒ Show (Max element)

instance semigroupMax
    ∷ Ord element ⇒ Semigroup (Max element) where
  append (Max this) (Max that) = Max (max this that)

instance monoidMax
    ∷ Bounded element ⇒ Monoid (Max element) where
  mempty = Max bottom

instance joinSemilatticeMax
    ∷ Bounded element ⇒ JoinSemilattice (Max element) where
  order = compare

instance arbitraryMax
    ∷ Arbitrary element ⇒ Arbitrary (Max element) where
  arbitrary = map Max arbitrary

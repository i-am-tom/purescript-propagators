module Data.Lattice.Max
  ( Max (..)
  ) where

import Data.Enum (class BoundedEnum, class Enum)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | The `Max` lattice keeps track of the "largest" value it has seen so far.
-- This might make sense, for example, if you're looking for the strongest
-- candidate from a set.
newtype Max (element ∷ Type) = Max element

derive         instance newtypeMax     ∷ Newtype (Max element) _
derive newtype instance eqMax          ∷ Eq      element     ⇒ Eq          (Max element)
derive newtype instance ordMax         ∷ Ord     element     ⇒ Ord         (Max element)
derive newtype instance showMax        ∷ Show    element     ⇒ Show        (Max element)
derive newtype instance enumMax        ∷ Enum    element     ⇒ Enum        (Max element)
derive newtype instance boundedMax     ∷ Bounded element     ⇒ Bounded     (Max element)
derive newtype instance boundedEnumMax ∷ BoundedEnum element ⇒ BoundedEnum (Max element)

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

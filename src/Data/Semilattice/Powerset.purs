module Data.Semilattice.Powerset where

import Data.Array as Array
import Data.Foldable (class Foldable, foldlDefault, foldMap, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Set (Set)
import Data.Set as Set
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Powerset (x ∷ Type)
  = Powerset (Set (Set x))

derive instance genericPowerset ∷ Generic (Powerset x) _
derive instance newtypePowerset ∷ Newtype (Powerset x) _
derive newtype instance eqPowerset ∷ Eq x ⇒ Eq (Powerset x)
derive newtype instance joinSemilatticePowerset ∷ Ord x ⇒ JoinSemilattice (Powerset x)
derive newtype instance monoidPowerset ∷ Ord x ⇒ Monoid (Powerset x)
derive newtype instance ordPowerset ∷ Ord x ⇒ Ord (Powerset x)
derive newtype instance showPowerset ∷ Show x ⇒ Show (Powerset x)

instance foldablePowerset ∷ Foldable Powerset where
  foldl f = foldlDefault f
  foldr f = foldrDefault f

  foldMap f (Powerset xs) = foldMap (foldMap f) xs

instance semigroupPowerset
    ∷ (Ord x, Semigroup x)
    ⇒ Semigroup (Powerset x) where
  append (Powerset this) (Powerset that) = Powerset do
    let combinations = (<>)
          <$> Array.fromFoldable this
          <*> Array.fromFoldable that

    this <> that <> Set.fromFoldable combinations

instance arbitraryPowerset
    ∷ (Arbitrary x, Ord x, JoinSemilattice x)
    ⇒ Arbitrary (Powerset x) where
  arbitrary = do
    objects ← map Set.fromFoldable (arbitrary ∷ _ (Array _))
    pure (foldMap singleton objects)

singleton ∷ ∀ x. x → Powerset x
singleton = Powerset <<< Set.singleton <<< Set.singleton

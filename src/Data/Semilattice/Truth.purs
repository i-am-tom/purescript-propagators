module Data.Semilattice.Truth where

import Data.Semilattice.Powerset (Powerset)
import Data.Semilattice.Provenance (Provenance (..))
import Data.Array as Array
import Data.Foldable (class Foldable, foldlDefault, foldMap, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Monoid (guard)
import Data.Semilattice.Join (class JoinSemilattice, order)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Truth (x ∷ Type)
  = Truth
      { ignored ∷ Set String
      , truths  ∷ Powerset (Provenance x)
      }

derive instance eqTruth ∷ Eq x ⇒ Eq (Truth x)
derive instance genericTruth ∷ Generic (Truth x) _
derive instance ordTruth ∷ Ord x ⇒ Ord (Truth x)
derive newtype instance joinSemilatticeTruth ∷ (JoinSemilattice x, Ord x) ⇒ JoinSemilattice (Truth x)
derive newtype instance monoidTruth ∷ (Monoid x, Ord x) ⇒ Monoid (Truth x)
derive newtype instance semigroupTruth ∷ (Ord x, Semigroup x) ⇒ Semigroup (Truth x)
derive newtype instance showTruth ∷ Show x ⇒ Show (Truth x)

instance foldTruth ∷ Foldable Truth where
  foldl f = foldlDefault f
  foldr f = foldrDefault f

  foldMap f (Truth { truths }) = foldMap (foldMap f) truths

instance arbitraryTruth
    ∷ (Arbitrary x, JoinSemilattice x, Ord x)
    ⇒ Arbitrary (Truth x) where
  arbitrary = do
    ignored ← map Set.fromFoldable (arbitrary ∷ _ (Array _))
    truths  ← arbitrary

    pure (Truth { ignored, truths })

listen ∷ ∀ x. Eq x ⇒ JoinSemilattice x ⇒ Truth x → x /\ Set (Set String) 
listen (Truth { truths, ignored }) = do
  let active = truths # foldMap \provenance@(Provenance { sources }) →
        guard (Set.subset ignored sources) [ provenance ]

      ordered = active # Array.sortBy \(Provenance this) (Provenance that) →
        order this.value that.value

      winner = Array.head ordered # foldMap \(Provenance this) →
        this.value

      matches = ordered # Array.takeWhile \(Provenance this) →
        this.value == winner

      authors = matches # foldMap \(Provenance this) →
        Set.singleton this.sources

  winner /\ authors

ignore ∷ String → Truth ~> Truth
ignore source (Truth archive)
  = Truth archive { ignored = Set.insert source archive.ignored }

unignore ∷ String → Truth ~> Truth
unignore source (Truth archive)
  = Truth archive { ignored = Set.delete source archive.ignored }

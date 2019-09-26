module Data.Semilattice.Backtrack where

import Data.Newtype (unwrap)
import Data.Foldable (foldMap)
import Data.Monoid (guard)
import Data.Generic.Rep (class Generic)
import Data.Monoid.Conj (Conj (..))
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Semilattice.Powerset (Powerset)
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)

newtype Backtrack (x ∷ Type)
  = Backtrack
      { branches  ∷ Powerset x
      , invariant ∷ x → Conj Boolean
      }

derive instance genericBacktrack ∷ Generic (Backtrack x) _
derive newtype instance joinSemilatticeBacktrack ∷ (JoinSemilattice x, Ord x) ⇒ JoinSemilattice (Backtrack x)
derive newtype instance monoidBacktrack ∷ (Monoid x, Ord x) ⇒ Monoid (Backtrack x)
derive newtype instance semigroupBacktrack ∷ (Ord x, Semigroup x) ⇒ Semigroup (Backtrack x)

instance arbitraryBacktrack
    ∷ (Arbitrary x, Coarbitrary x, JoinSemilattice x, Ord x)
    ⇒ Arbitrary (Backtrack x) where
  arbitrary = do
    branches  ← arbitrary
    invariant ← map (Conj <<< _) arbitrary

    pure (Backtrack { branches, invariant })

listen ∷ ∀ x. Eq x ⇒ JoinSemilattice x ⇒ Backtrack x → x
listen (Backtrack { branches, invariant })
  = branches # foldMap \x → guard (unwrap (invariant x)) x

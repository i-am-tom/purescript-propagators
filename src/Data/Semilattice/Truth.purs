module Data.Semilattice.Truth
  ( Truth

  , assert
  , assert'
  , read
  , reintroduce
  , retract
  ) where

import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Data.Foldable (class Foldable, null)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple (..))
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

-- A truth maintenance system not only holds a value, but keeps track of _how_
-- that value was calculated. This means that, after the fact, we can inspect
-- how values change when we remove certain inputs. As an example, if we arrive
-- at a contradiction, we may want to debug the contradiction by retracting
-- certain information.
newtype Truth (element ∷ Type)
  = Truth (Map (Set Int) element)

derive instance newtypeTruth ∷ Newtype (Truth element) _
derive newtype instance eqTruth ∷ Eq element ⇒ Eq (Truth element)
derive newtype instance foldableTruth ∷ Foldable Truth
derive newtype instance foldableWithIndexTruth ∷ FoldableWithIndex (Set Int) Truth
derive newtype instance functorTruth ∷ Functor Truth
derive newtype instance ordTruth ∷ Ord element ⇒ Ord (Truth element)
derive newtype instance showTruth ∷ Show element ⇒ Show (Truth element)

-- An assertion is the easiest way to create a `Truth`. Strictly, an assertion
-- holds two truths: the one you supply (these IDs gave me this information),
-- and the "empty truth" (no IDs told me the most vague possible description).
assert' ∷ ∀ x. JoinSemilattice x ⇒ Set Int → x → Truth x
assert' origin fact = Truth $ Map.fromFoldable
  [ Tuple mempty mempty -- Not knowing anything.
  , Tuple origin fact   -- Knowing our fact.
  ]

-- When we assert truths in our "god mode", we don't associate our truths with
-- identifiers, and so this is a convenience.
assert ∷ ∀ x. JoinSemilattice x ⇒ x → Truth x
assert = assert' mempty

-- To "learn" a fact is to aggregate it into our truth. Its provenance is
-- tracked, as well as its consequences, so we can retract and reintroduce
-- learnings as we please.
learn ∷ ∀ x. JoinSemilattice x ⇒ Map (Set Int) x → Set Int → x → Map (Set Int) x
learn archive reporters report = do
  let aggregate ∷ Set Int → x → Map (Set Int) x
      aggregate origin fact
        = Map.singleton (reporters <> origin) (report <> fact)

  Map.unionWith append archive                              -- What we knew.
    $ Map.unionWith append (Map.singleton reporters report) -- What we now know.
    $ foldMapWithIndex aggregate archive                    -- What we've learned.

instance semigroupTruth ∷ JoinSemilattice x ⇒ Semigroup (Truth x) where
  append (Truth this) (Truth that)
    = Truth (foldMapWithIndex (\key value → learn that key value) this)

instance monoidTruth ∷ JoinSemilattice x ⇒ Monoid (Truth x) where
  mempty = Truth (Map.singleton mempty mempty)

instance joinSemilatticeTruth ∷ JoinSemilattice x ⇒ JoinSemilattice (Truth x)

instance arbitraryTruth
    ∷ (Arbitrary x, JoinSemilattice x) ⇒ Arbitrary (Truth x) where
  arbitrary = do
    origin ← map Set.singleton arbitrary
    fact   ← arbitrary

    pure (assert' origin fact)

-- To read a value from a truth, we first filter out all information based on
-- origins we've retracted. After that, we just fold the remainder together.
-- The result is the best description of an answer according to the information
-- we're considering.
read ∷ ∀ m r x. JoinSemilattice x ⇒ MonadState { retractions ∷ Set Int | r } m ⇒ Truth x → m x
read (Truth archive) = do
  { retractions } ← State.get

  pure $ archive # foldMapWithIndex \key value →
    case Set.intersection key retractions of
      xs | null xs → value
      _            → mempty

-- We can choose to "retract" truths by their identifier. When a retraction has
-- been made, that identifier's contributions to other values is withdrawn, and
-- we're hence left with a network presented "as though that value hadn't
-- existed".
retract ∷ ∀ m r. MonadState { retractions ∷ Set Int | r } m ⇒ Int → m Unit
retract addition = void $ State.modify \context@{ retractions } → do
  context { retractions = Set.singleton addition <> retractions }

-- We can revert a retraction, which gives us a nice, maybe even interactive
-- way of viewing effects within our network.
reintroduce ∷ ∀ m r. MonadState { retractions ∷ Set Int | r } m ⇒ Int → m Unit
reintroduce addition = void $ State.modify \context@{ retractions } → do
  context { retractions = Set.delete addition retractions }

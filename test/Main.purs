module Test.Main where

import Prelude

import Data.Lattice as L
import Data.Lattice.Defined as D
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.QuickCheck (class Arbitrary, (===), (/==))
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Type.Proxy (Proxy (..))

class (Arbitrary t, Eq t, Show t) ⇐ Testable t
instance testableAlias ∷ (Arbitrary t, Eq t, Show t) ⇒ Testable t

main :: Effect Unit
main = run [ consoleReporter ] do
  describe "Data.Lattice" do
    -- TODO: pick more interesting types than `Unit`.
    describe "Unit"    do jsl (Proxy ∷ Proxy                               Unit )
    describe "Defined" do jsl (Proxy ∷ Proxy                     (D.Defined Int))
    describe "Pair"    do jsl (Proxy ∷ Proxy (D.Defined Int /\ D.Defined String))

jsl ∷ ∀ t. L.JoinSemilattice t ⇒ Testable t ⇒  Proxy t → Spec Unit
jsl _ = do
  it "is associative" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    x <> (y <> z) === (x <> y) <> z

  it "is monoidal" $ quickCheck \(x ∷ t) →
    mempty <> x === x

  it "is idempotent" $ quickCheck \(x ∷ t) (y ∷ t) →
    x <> y <> y === x <> y

  it "is commutative" $ quickCheck \(x ∷ t) (y ∷ t) →
    x <> y === y <> x

  it "is ordered" $ quickCheck \(x ∷ t) (y ∷ t) →
    if x <> y == x
      then L.order (x <> y) x /== GT
      else L.order (x <> y) x === GT

module Test.Main where

import Prelude

import Data.Lattice as L
import Data.Lattice.Defined (Defined)
import Data.Lattice.Defined as Defined
import Data.Propagator as Prop
import Data.Propagator.Cell as Cell
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, (===), (/==))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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
    describe "Unit" do jsl (Proxy ∷ Proxy Unit )
    describe "Defined" do jsl (Proxy ∷ Proxy (Defined Int))
    describe "Pair" do jsl (Proxy ∷ Proxy (Defined Int /\ Defined String))

  describe "Data.Propagator" do
    let cToF
          ∷ ∀ m c
          . Cell.MonadCell c m
          ⇒ Prop.Propagator m c (Defined Number)
          → Prop.Propagator m c (Defined Number)
        cToF x = x * Prop.emit (Defined.Known (9.0 / 5.0))
                   + Prop.emit (Defined.Known 32.0)

    it "runs computations 'forwards'" do
      Prop.forwards_ cToF (Defined.Known 5.0)
        `shouldEqual` Defined.Known 41.0

    it "runs computations 'backwards'" do
      Prop.backwards_ cToF (Defined.Known 41.0)
        `shouldEqual` Defined.Known 5.0

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

module Test.Main where

import Prelude

import Data.Int as Int
import Data.Lattice as L
import Data.Lattice.Defined (Defined)
import Data.Lattice.Defined as Defined
import Data.Propagator (Propagator)
import Data.Propagator as Propagator
import Data.Propagator.Cell (class MonadCell)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, (===), (/==))
import Test.Spec (Spec, describe, it)
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
    -- It's a bit ugly without the polymorphic numbers that Haskell enjoys...
    let celsiusToFahrenheit ∷ ∀ c m
                            . MonadCell c m
                            ⇒ Propagator m c (Defined Number)
                            → Propagator m c (Defined Number)
        celsiusToFahrenheit x
          = x * Propagator.emit (Defined.Known  9.0)
              / Propagator.emit (Defined.Known  5.0)
              + Propagator.emit (Defined.Known 32.0)

    it "executes 'forwards'" $ quickCheck \x → do
      let input = Int.toNumber x

          result
            = Propagator.forwards_ celsiusToFahrenheit
            $ Defined.Known input

      result === Defined.Known (input * 9.0 / 5.0 + 32.0)

    it "executes 'backwards'" $ quickCheck \x → do
      let output = Int.toNumber x

          result
            = Propagator.backwards_ celsiusToFahrenheit
            $ Defined.Known output

      result === Defined.Known ((output - 32.0) * 5.0 / 9.0)

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

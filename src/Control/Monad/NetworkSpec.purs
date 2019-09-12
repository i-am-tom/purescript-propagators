module Control.Monad.NetworkSpec where

import Control.Monad.Network (Cell)
import Control.Monad.Network as Network
import Data.Ord.Max (Max (..))
import Data.Semilattice.Truth as Truth
import Data.Tuple.Nested ((/\))
import Prelude
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Control.Monad.NetworkSpec" do
  it "creates unique identifiers" do
    let this /\ that = Network.run do
          this ∷ Cell _ (Max Int) ← Network.make
          that ∷ Cell _ (Max Int) ← Network.make

          pure $ Network.identifier this
              /\ Network.identifier that

    this `shouldNotEqual` that

  it "creates zero-knowledge cells" do -- Parametricity?
    let fresh = Network.run do
          this ∷ Cell _ (Max Int) ← Network.make
          Network.read this

    fresh `shouldEqual` mempty

  it "collects knowledge" $ quickCheck \(x ∷ Int) → do
    let result = Network.run do
          fresh ← Network.make -- Max minBound

          Network.write fresh (Truth.assert (Max x))
          Network.read  fresh -- Max minBound <> Max x

    result === Max x

  it "lifts unary functions" $ quickCheck \(x ∷ Int) → do
    let result = Network.run do
          input  ← Network.make
          output ← Network.make

          let go (Max z) = Max (10 + z)
          Network.unary go input output

          Network.write input (Truth.assert (Max x))
          Network.read  output

    result === Max (x + 10)

  it "lifts binary functions" do
    let result = Network.run do
          left   ← Network.make
          right  ← Network.make
          output ← Network.make

          let go (Max a) (Max b) = Max (a + b)
          Network.binary go left right output

          Network.write left  (Truth.assert (Max 5))
          Network.write right (Truth.assert (Max 7))
          Network.read  output

    result `shouldEqual` Max 12

  it "is bidirectional" do
    let result = Network.run do
          input  ← Network.make
          output ← Network.make

          let to  (Max z) = Max (10 + z)
          let fro (Max z) = Max (z - 10)

          Network.unary to  input  output
          Network.unary fro output input

          Network.write output (Truth.assert (Max 25))
          Network.read  input

    result `shouldEqual` Max 15

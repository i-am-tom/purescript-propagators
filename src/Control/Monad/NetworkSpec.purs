module Control.Monad.NetworkSpec where

import Control.Monad.Network (Cell)
import Control.Monad.Network as Network
import Control.Monad.ST as ST
import Data.Ord.Max (Max (..))
import Prelude
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Control.Monad.NetworkSpec" do
  it "creates zero-knowledge cells" do -- Parametricity?
    let fresh = ST.run do
          this ∷ Cell _ (Max Int) ← Network.make
          Network.read this

    fresh `shouldEqual` mempty

  it "collects knowledge" $ quickCheck \(x ∷ Int) → do
    let result = ST.run do
          fresh ← Network.make -- Max minBound

          Network.write fresh (Max x)
          Network.read  fresh -- Max minBound <> Max x

    result === Max x

  it "lifts unary functions" $ quickCheck \(x ∷ Int) → do
    let result = ST.run do
          input  ← Network.make
          output ← Network.make

          let go (Max z) = Max (10 + z)
          Network.unary go input output

          Network.write input (Max x)
          Network.read  output

    result === Max (x + 10)

  it "lifts binary functions" do
    let result = ST.run do
          left   ← Network.make
          right  ← Network.make
          output ← Network.make

          let go (Max a) (Max b) = Max (a + b)
          Network.binary go left right output

          Network.write left  (Max 5)
          Network.write right (Max 7)
          Network.read  output

    result `shouldEqual` Max 12

  it "is bidirectional" do
    let result = ST.run do
          input  ← Network.make
          output ← Network.make

          let to  (Max z) = Max (10 + z)
          let fro (Max z) = Max (z - 10)

          Network.unary to  input  output
          Network.unary fro output input

          Network.write output (Max 25)
          Network.read  input

    result `shouldEqual` Max 15

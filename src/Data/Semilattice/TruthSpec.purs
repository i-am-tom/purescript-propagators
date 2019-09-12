module Data.Semilattice.TruthSpec where

import Control.Monad.State (evalState)
import Data.Semilattice.Interval (Interval)
import Data.Semilattice.Truth (Truth)
import Data.Semilattice.Truth as Truth
import Prelude
import Test.Laws as Laws
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy (..))

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Truth" do
  Laws.with (Proxy ∷ Proxy (Truth (Interval Int)))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

  describe "assert" do
    it "preserves the fact" $ quickCheck \(x ∷ Interval Int) → do
      evalState (Truth.read (Truth.assert x)) { retractions: mempty } === x

    it "retracts and reintroduces" $ quickCheck \(x ∷ Truth (Interval Int)) (y ∷ Int) → do
      let program = do
            before ← Truth.read x

            Truth.retract y
            Truth.reintroduce y
      
            after ← Truth.read x

            pure { before, after }
      
      let results = evalState program { retractions: mempty }
      results.before === results.after

module Main where

import Data.Ord.Max (Max (..))
import Effect (Effect)
import Effect.Console as Console
import Control.Monad.Network as Network
import Data.Propagator (Propagator, emit, backwards)
import Prelude

main ∷ Effect Unit
main = do
  let go ∷ ∀ r. Propagator r (Max Int) → Propagator r (Max Int)
      go x = x <> emit (Max 10)

  Console.log (show (Network.run (backwards go (Max 5))))

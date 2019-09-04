module Visual.Trace.Context where

import Data.Newtype (class Newtype)

-- | The context of the `Trace` monad. Really, this is just the set of
-- variables that allow us to create accurate logs of the interactions within a
-- network.
newtype Context
  = Context
      { supply  ∷ Int
      , watcher ∷ Array Int
      }

derive instance newtypeContext ∷ Newtype Context _

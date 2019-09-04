module Visual.Main where

import Graphics.Drawing (Drawing)
import Control.Monad.ST (run)
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer.Trans (execWriterT)
import FRP.Behavior (Behavior)
import Prelude
import Visual.Trace (Trace (..))
import Visual.Trace.Context (Context (..))
import Visual.Trace.Event (Event)

visualise ∷ ∀ a. (∀ r. Trace r a) → Behavior Drawing
visualise (Trace action) = do
  let initial ∷ Context
      initial = Context
        { supply: 0
        , watcher: []
        }

      logs ∷ Array Event
      logs = run (execWriterT (evalStateT action initial))

  pure mempty
  

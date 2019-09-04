module Test.Trace.Event where

import Data.Maybe (Maybe, maybe)
import Prelude

-- | Propagator network events.
--
-- This is not very in-depth, and serves simply as a way to give us a
-- vocabulary for visualising network traffic.
data Event
  
  -- | A new node has been added to the network. We assign an ID so that we can
  -- talk about it in relation to other nodes.
  = Make Int

  -- | A write has been made to a node. If the source list is empty, this is
  -- assumed to be an external write, rather than the influence of other nodes.
  -- The list of sources is effectively a stack trace of where the writes
  -- originated. In other words, if a watcher triggers a watcher that causes a
  -- write, we can see which node the first watcher was watching. That's quite
  -- a sentence.
  | Write
      { payload ∷ Drawing
      , source  ∷ Array Int
      , target  ∷ Int
      }

  -- | Similarly, we can see the context in which a read happens. Note that we
  -- can use empty source lists to indicate, say, inputs and outputns
  -- graphically.
  | Read
      { source ∷ Array Int
      , target ∷ Int
      }

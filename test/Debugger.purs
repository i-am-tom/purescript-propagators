module Test.Debugger where

-- import Graphics.Drawing (Drawing)
import Control.Monad.ST (run)
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer.Trans (execWriterT)
import Data.Lattice.Defined (Defined)
import Data.Lattice.Defined as Defined
import Data.Propagator as Prop
import Prelude
import Test.Trace (Trace (..), Cell)
import Test.Trace.Context (Context (..))
import Test.Trace.Event (Event)

logs ∷ ∀ a. (∀ r. Trace r a) → Array Event
logs (Trace action)
  = run (execWriterT (evalStateT action initial))
  where
    initial ∷ Context
    initial = Context
      { supply: 0
      , watcher: []
      }

example
  ∷ ∀ r
  . Prop.Propagator (Trace r) (Cell r) (Defined Number)
  → Prop.Propagator (Trace r) (Cell r) (Defined Number)
example x
  = x * Prop.emit (Defined.Known  9.0)
      / Prop.emit (Defined.Known  5.0)
      + Prop.emit (Defined.Known 32.0)

-- > to
-- [ New cell (0)
-- , New cell (1)
-- , New cell (2)
-- , New cell (3)
-- , I wrote {"value0":1.8} to (3)
-- , New cell (4)
-- , I wrote {"value0":32} to (4)
-- , I wrote {"value0":5} to (0)
-- , (0) wrote {"value0":9} to (2)
-- , (2) wrote {"value0":41} to (1)
-- ]
to ∷ Array Event
to = logs (Prop.forwards example (Defined.Known 5.0))

-- > fro
-- [ New cell (0)
-- , New cell (1)
-- , New cell (2)
-- , New cell (3)
-- , I wrote {"value0":1.8} to (3)
-- , New cell (4)
-- , I wrote {"value0":32} to (4)
-- , I wrote {"value0":41} to (1)
-- , (1) wrote {"value0":9} to (2)
-- , (2) wrote {"value0":5} to (0)
-- ]
fro ∷ Array Event
fro = logs (Prop.backwards example (Defined.Known 41.0))

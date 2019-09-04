module Test.Trace where

import Control.Apply (lift2)
import Control.Monad.ST (ST)
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as ST
import Control.Monad.State.Class (class MonadState, get, modify)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array ((:), head)
import Data.Foldable (for_)
import Data.Lattice (implies)
import Data.Newtype (over)
import Data.Propagator.Cell as Cell
import Global.Unsafe (unsafeStringify)
import Prelude
import Test.Trace.Context (Context (..))
import Test.Trace.Event (Event (..))

-- | The `Trace` monad is a working implementation of `MonadCell` with two
-- important additions:
--
-- * A `State` for carrying the "next identifier" (should we make a new cell),
--   and the IDs of the callback (if any) that we're currently running. This
--   allows us to detect the "path" of changes.
--
-- * A `Writer` which uses the information in the state to log a record of how
-- data flows throughout the network.
--
-- Ideally, I would have liked this to be a transformer, but I ran into some
-- problems with `watch`: namely, if I call the underlying `watch` function, I
-- can't use `State` or `Writer` in that callback, which I would need in order
-- to detect nested writes. Still, if anyone can work this out, I'd be super
-- grateful!
newtype Trace (region ∷ Region) (value ∷ Type)
  = Trace (StateT Context (WriterT (Array Event) (ST region)) value)

derive newtype instance applicativeTrace ∷ Applicative (Trace r)
derive newtype instance applyTrace       ∷ Apply (Trace r)
derive newtype instance bindTrace        ∷ Bind (Trace r)
derive newtype instance functorTrace     ∷ Functor (Trace r)
derive newtype instance monadStateTrace  ∷ MonadState Context (Trace r)
derive newtype instance monadTellTrace   ∷ MonadTell (Array Event) (Trace r)
derive newtype instance monadTrace       ∷ Monad (Trace r)

-- | If this _were_ a monad transformer, this would be the `lift`
-- implementation. Take an `ST` computation, and lift it into `Trace`. Note
-- that this can't retroactively add logging to anything within that
-- computation.
promote ∷ ∀ r. ST r ~> Trace r
promote = Trace <<< lift <<< lift

instance semigroupTrace ∷ Semigroup a ⇒ Semigroup (Trace r a) where
  append = lift2 append

instance monoidTrace ∷ Monoid a ⇒ Monoid (Trace r a) where
  mempty = pure mempty

-- | Similar to the `ST` cell, we can have our own trackable cell. It's really
-- the same thing, but with a more capable callback and the assigned identifier
-- attached.
newtype Cell (region ∷ Region) (content ∷ Type)
  = Cell
      { identifier ∷ Int
      , ref ∷ STRef region
          { content  ∷ content
          , onChange ∷ content → Trace region Unit
          }
      }

instance monadCellTrace ∷ Cell.MonadCell (Cell r) (Trace r) where
  make = do
    -- Create a new identifier for the node.

    Context { supply: identifier } ← get

    _   ← modify (over Context _ { supply = identifier + 1 })
    ref ← promote $ ST.new { content: mempty, onChange: mempty }

    tell -- Log the creation.
      [ Make { identifier }
      ]

    pure (Cell { identifier, ref })

  read (Cell { identifier: source, ref }) = do
    Context { watcher } ← get
    { content }         ← promote (ST.read ref)

    for_ (head watcher) \target ->
      tell -- If we /are/ in a callback, this "read" can be shown as a "write"
           -- in the opposite direction.
        [ Write
            { payload: unsafeStringify content
            , source: source : watcher
            , target
            }
        ]

    pure content 

  watch (Cell { identifier, ref }) callback = do
    { content, onChange } ← promote (ST.read ref)

    _ ← ref # promote <<< ST.modify _
      { onChange = \new → do
          -- Add the latest "watcher", and remove it after the callback. This
          -- way we can see how one update affects others.

          Context { watcher } ← get

          _ ← modify (over Context _ { watcher = identifier : watcher })
          _ ← callback new
          _ ← modify (over Context _ { watcher = watcher })

          onChange new
      }

    callback content

  write (Cell { identifier, ref }) next = do
    { content, onChange } ← promote (ST.read ref)

    unless (content `implies` next) do
      let joined = content <> next

      _ ← ref # promote <<< ST.modify _
        { content = joined
        }

      Context { watcher } ← get

      tell
        [ Write
            { payload: unsafeStringify joined
            , source: watcher
            , target: identifier
            }
        ]

      onChange joined

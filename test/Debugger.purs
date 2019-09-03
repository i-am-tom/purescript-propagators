module Test.Debugger where

import Control.Apply (lift2)
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as ST
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.State.Class (class MonadState, get, modify)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, execWriterT)
import Data.Foldable (for_)
import Data.Newtype (class Newtype, over)
import Data.Lattice (implies)
import Data.Lattice.Defined (Defined)
import Data.Lattice.Defined as Defined
import Data.Propagator.Cell as Cell
import Data.Propagator as Prop
import Data.Maybe (Maybe (..), maybe)
-- import Graphics.Drawing (Drawing)
import Prelude

import Global.Unsafe (unsafeStringify)

data Event
  = Make
      { identifier ∷ Int
      }

  | Write
      { payload ∷ String
      , source  ∷ Maybe Int
      , target  ∷ Int
      }

instance showEvent ∷ Show Event where
  show = case _ of
    Make { identifier } →
      "New cell (" <> show identifier <> ")"

    Write { payload, source, target } → do
      let src = maybe "I" (\x → "(" <> show x <> ")") source

      src <> " wrote " <> payload <> " to (" <> show target <> ")"

newtype Context
  = Context
      { supply  ∷ Int
      , watcher ∷ Maybe Int 
      }

derive instance newtypeContext ∷ Newtype Context _

newtype Test (region ∷ Region) (value ∷ Type)
  = Test (StateT Context (WriterT (Array Event) (ST region)) value)

promote ∷ ∀ r. ST r ~> Test r
promote = Test <<< lift <<< lift

derive newtype instance applicativeTest ∷ Applicative (Test r)
derive newtype instance applyTest       ∷ Apply (Test r)
derive newtype instance bindTest        ∷ Bind (Test r)
derive newtype instance functorTest     ∷ Functor (Test r)
derive newtype instance monadStateTest  ∷ MonadState Context (Test r)
derive newtype instance monadTellTest   ∷ MonadTell (Array Event) (Test r)
derive newtype instance monadTest       ∷ Monad (Test r)

instance semigroupTest ∷ Semigroup a ⇒ Semigroup (Test r a) where
  append = lift2 append

instance monoidTest ∷ Monoid a ⇒ Monoid (Test r a) where
  mempty = pure mempty

fresh ∷ ∀ r. Test r Int
fresh = do
  Context { supply } ← get
  _                  ← modify (over Context _ { supply = supply + 1 })

  pure supply

log ∷ ∀ r. Event → Test r Unit
log = tell <<< pure

focus ∷ ∀ r. Int → Test r Unit → Test r Unit
focus source callback = do
  Context { watcher } ← get

  _ ← modify (over Context _ { watcher = Just source })
  _ ← callback
  _ ← modify (over Context _ { watcher = watcher })

  pure unit

newtype Cell (region ∷ Region) (content ∷ Type)
  = Cell
      { identifier ∷ Int
      , ref ∷ STRef region
          { content  ∷ content
          , onChange ∷ content → Test region Unit
          }
      }

instance monadCellTest ∷ Cell.MonadCell (Cell r) (Test r) where
  make = do
    identifier ← fresh
    ref        ← promote $ ST.new { content: mempty, onChange: mempty }

    log (Make { identifier })
    pure (Cell { identifier, ref })

  read (Cell { identifier: source, ref }) = do
    Context { watcher } ← get
    { content }         ← promote (ST.read ref)

    for_ watcher \target →
      log $ Write
        { payload: unsafeStringify content
        , source: Just source
        , target
        }

    pure content 

  watch (Cell { identifier, ref }) callback = do
    { content, onChange } ← promote (ST.read ref)

    _ ← ref # promote <<< ST.modify _
      { onChange = \new → do
          focus identifier (callback new)
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

      log $ Write
        { payload: unsafeStringify joined
        , source: watcher
        , target: identifier
        }

      onChange joined

logs ∷ ∀ a. (∀ r. Test r a) → Array Event
logs (Test action)
  = run (execWriterT (evalStateT action initial))
  where
    initial ∷ Context
    initial = Context
      { supply: 0
      , watcher: Nothing
      }

example
  ∷ ∀ r
  . Prop.Propagator (Test r) (Cell r) (Defined Number)
  → Prop.Propagator (Test r) (Cell r) (Defined Number)
example x = x * Prop.emit (Defined.Known (9.0 / 5.0))
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

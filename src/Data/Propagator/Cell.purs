module Data.Propagator.Cell where

import Prelude
import Data.Lattice (class JoinSemilattice)

-- | `MonadCell` is the set of primitives we require in order to specify a
-- propagator network within our monad of choice. Note that this API is quite
-- stateful: `watch` should register an observer, and `write` should notify
-- these observers. It's therefore easiest to impliment this with `IO` or `ST`,
-- though I'm sure there's probably some elegant solution using `State`.
--
-- _Most importantly for our purposes, we use this generalisation in order to
-- build visualisations of our propagator networks._
class Monad context ⇐ MonadCell (cell ∷ Type → Type) (context ∷ Type → Type)
    | context → cell where

  -- | Make a new cell with `mempty` ("bottom" in lattice-speak) contents. Its
  -- initial state should have no observers.
  make ∷ ∀ content. context (cell content)

  -- | Read from a cell. Any non-`mempty` result implies that something has
  -- been written to this cell successfully.
  read ∷ ∀ content. cell content → context content

  -- | Watch a cell for changes. When this cell changes, the given callback
  -- will be executed. This callback may involve writing to other cells, which
  -- is how we build computations.
  --
  -- This is a _primitive_, and its use should be discouraged in favour of an
  -- appropriate propagator for more flexibility.
  watch ∷ ∀ content. cell content → (content → context Unit) → context Unit

  -- | Write to a cell. A write should only notify observers if the write's
  -- data isn't implied by the current value. In other words, writes should
  -- only be observably "successful" when the write offers information
  -- previously unknown.
  write
    ∷ ∀ content
    . JoinSemilattice content
    ⇒ cell content
    → content
    → context Unit

-- | Often, we want to initialise cells with a value. This doesn't clear the
-- Fairbairn threshold by much, but tidies up a lot of clutter.
fill
  ∷ ∀ cell content context
  . JoinSemilattice content
  ⇒ MonadCell cell context
  ⇒ content
  → context (cell content)

fill value = do
  fresh <- make
  write fresh value

  pure fresh

-- | A function `a → b` can be "lifted" over two cells, but note that this does
-- not automatically establish any possible inversions. It's therefore sensible
-- to use an available propagator if possible, as this will improve knowledge
-- sharing within the network.
lift
  ∷ ∀ cell context source target
  . JoinSemilattice source
  ⇒ JoinSemilattice target
  ⇒ MonadCell cell context
  ⇒ (source → target)
  → cell source
  → cell target
  → context Unit

lift f source target
  = watch source \x → write target (f x)

-- | A function with more than one argument can also be lifted, with the same
-- caveats as `lift`. Think of these two functions as `<$>` and `<*>`, so you
-- can lift n-ary functions into a cell context.
lift2
  ∷ ∀ cell context this that target
  . JoinSemilattice this
  ⇒ JoinSemilattice that
  ⇒ JoinSemilattice target
  ⇒ MonadCell cell context
  ⇒ (this → that → target)
  → cell this
  → cell that
  → cell target
  → context Unit

lift2 f this that target
  = watch2 this that \x y → write target (f x y)

-- | If cell `X` writes every change in `Y` to itself, and `Y` does the
-- opposite, then these cells will hold identical values once propagation has 
-- ceased. As such, `unify` can be seen as a function that takes two cells and
-- replaces them with a shared cell.
unify
  ∷ ∀ cell context content
  . JoinSemilattice content
  ⇒ MonadCell cell context
  ⇒ cell content
  → cell content
  → context Unit

unify this that = do
  watch this (write that)
  watch that (write this)

-- | We're not restricted to watching only one cell: we can watch many, with a
-- `<$>` and `<*>` parallel similar to `lift` and `lift2`. Note again that this
-- doesn't set up any possible inversions, and so any available and appropriate
-- propagator will probably give you better knowledge-sharing.
watch2
  ∷ ∀ cell context this that
  . MonadCell cell context
  ⇒ cell this
  → cell that
  → (this → that → context Unit)
  → context Unit

watch2 this that onChange = do
  watch this \x → watch that \y → onChange x y
  watch that \y → watch this \x → onChange x y

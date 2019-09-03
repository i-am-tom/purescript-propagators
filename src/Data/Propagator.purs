module Data.Propagator where

import Control.Monad.ST (ST, run)
import Data.Exists (Exists, mkExists, runExists)
import Data.HeytingAlgebra (tt, ff, implies)
import Data.Lattice (class JoinSemilattice)
import Data.Propagator.Cell (class MonadCell, Cell)
import Data.Propagator.Cell as Cell
import Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | A propagator is a relationship _between_ cells. Strictly, the relationship
-- must be monotonic in all active directions. Wait, did I say directions
-- _plural_?
--
-- Because we are effectively writing some auto-wiring between a set of cells,
-- there's no reason why a function has to go only from input to output: if
-- it be injective, we could go from output to input! Within the propagator's
-- internals, we can describe as many relationships as we like, and then
-- compose propagators to build networks of knowledge-sharing cells.
--
-- Propagators form very, very interesting instances for many of our standard
-- classes. Namely, because we have access to the output cell when wiring up,
-- we can add in as many relationships as we can establish. So, for example, `a
-- + b = c` also gives us `c - a = b` and `c - b = a` for free. Thus, when you
-- establish a `(+)` relationship between three cells, you will always get the
-- third value given any two.
data Propagator (context ∷ Type → Type) (cell ∷ Type → Type) (output ∷ Type)

  -- | A nullary propagator has no "inputs". Really, we could call it an
  -- "emitter": it describes a single cell that can be entirely standalone.
  = Nullary (context (cell output))

  -- | A unary propagator is a relationship between two cells, which we may
  -- traditionally have labelled "input" and "output". As we'll see, however,
  -- there's not always a good reason why we should have to designate a single
  -- direction to the relationship. We existentialise one of the cells, simply
  -- because it gives us access to a few typeclasses that may become useful.
  | Unary (Exists (Unary_ context cell output))

  -- | Finally, the binary propagator gives us a way of building functions
  -- between any number of cells. Here, we can specify more complex
  -- relationships, while enjoying composition of networks.
  | Binary (Exists2 (Binary_ context cell output))

-- | A non-existential Unary propagator has two cells, which we've crudely
-- named "input" and "output" (though, as we will see, these names are
-- meaningless in a lot of cases).
--
-- Note that we provide a pre-made cell for this, as the absence of GADTs means
-- we can't package up a proof of `JoinSemilattice output`. Given that this only
-- causes a problem in `lower` when we want to make an output cell, we just
-- include the output cell in the constructor.
data Unary_ context cell output input
  = Unary_ (cell input → cell output → context Unit)
           (context (cell output))
           (Propagator context cell input)

-- | A non-existential Binary propagator, which we can use to express a
-- relationship between three cells. We call it "binary" to adhere to the
-- traditional intuition of a binary function having "two inputs", rather than
-- being a relationship between "three values".
--
-- Again, we must include this pre-made output cell to avoid the problems
-- caused by an absence of `JoinSemilattice output` proof.
data Binary_ context cell output this that
  = Binary_ (cell this → cell that → cell output → context Unit)
            (context (cell output))
            (Propagator context cell this) 
            (Propagator context cell that)

-- | Short-hand for construction of a nullary propagator. This doesn't need to
-- exist as this is an alias for the Nullary constructor, but it's here for
-- consistency.
nullary
  ∷ ∀ context cell output
  . context (cell output)
  → Propagator context cell output

nullary = Nullary

-- | Short-hand for construction of a unary propagator. This gives us a way of
-- lifting a two-cell relationship into a propagator.
unary
  ∷ ∀ context cell input output
  . JoinSemilattice output
  ⇒ MonadCell cell context
  ⇒ (cell input → cell output → context Unit)
  → Propagator context cell input
  → Propagator context cell output

unary go = Unary <<< mkExists <<< Unary_ go Cell.make

-- | Short-hand for construction of a binary propagator. With `unary` and
-- `binary`, we have enough vocabulary to express relationships between any
-- number of cells.
binary
  ∷ ∀ context cell this that output
  . JoinSemilattice output
  ⇒ MonadCell cell context
  ⇒ (cell this → cell that → cell output → context Unit)
  → Propagator context cell this
  → Propagator context cell that
  → Propagator context cell output

binary go this that
  = Binary (mkExists2 (Binary_ go Cell.make this that))

instance semiringPropagator
    ∷ ( Eq content
      , EuclideanRing content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ Semiring (Propagator context cell content) where

  add = binary \x y z → do
    Cell.lift2 (+) x y z
    Cell.lift2 (-) z x y
    Cell.lift2 (-) z y x

  mul = binary \x y z → do
    Cell.lift2 (*) x y z

    Cell.watch z \c →
      if c == zero then do
        Cell.watch x \a → when (a /= zero) do Cell.write y zero
        Cell.watch y \b → when (b /= zero) do Cell.write x zero
      else do
        Cell.watch x \a → Cell.write y (c / a)
        Cell.watch y \b → Cell.write x (c / b)

  one  = Nullary (Cell.fill  one)
  zero = Nullary (Cell.fill zero)

instance ringPropagaor
    ∷ ( Eq content
      , EuclideanRing content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ Ring (Propagator context cell content) where
  sub = binary \x y z → do
    Cell.lift2 (-) x y z
    Cell.lift2 (-) x z y
    Cell.lift2 (+) z y x

instance commutativeRingPropagator
    ∷ ( Eq content
      , CommutativeRing content
      , EuclideanRing content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ CommutativeRing (Propagator context cell content)

instance divisionRingPropagator
    ∷ ( DivisionRing content
      , Eq content
      , EuclideanRing content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ DivisionRing (Propagator context cell content) where
  recip = unary \x y → do
    Cell.lift recip x y
    Cell.lift recip y x

instance euclideanRingPropagator
    ∷ ( Eq content
      , EuclideanRing content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ EuclideanRing (Propagator context cell content) where
  degree _ = 1

  div = binary \z y x → do
    Cell.lift2 (*) x y z

    Cell.watch z \c →
      if c == zero then do
        Cell.watch x \a → when (a /= zero) do Cell.write y zero
        Cell.watch y \b → when (b /= zero) do Cell.write x zero
      else do
        Cell.watch x \a → Cell.write y (c / a)
        Cell.watch y \b → Cell.write x (c / b)

  mod = binary (Cell.lift2 mod)

instance semigroupPropagator
    ∷ ( JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ Semigroup (Propagator context cell content) where
  append = binary (Cell.lift2 append)

instance monoidPropagator
    ∷ ( JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ Monoid (Propagator context cell content) where
  mempty = Nullary (Cell.fill mempty)

instance booleanAlgebraPropagator
    ∷ ( BooleanAlgebra content
      , Eq content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ BooleanAlgebra (Propagator context cell content)

instance heytingAlgebraPropagator
    ∷ ( Eq content
      , HeytingAlgebra content
      , JoinSemilattice content
      , MonadCell cell context
      )
    ⇒ HeytingAlgebra (Propagator context cell content) where
  ff = Nullary (Cell.fill ff)
  tt = Nullary (Cell.fill tt)

  implies = binary \x y z → do
    Cell.lift2 implies x y z

    Cell.watch2 x z \a c → when          (a == tt) do Cell.write y      c
    Cell.watch2 y z \b c → unless ((b && c) == tt) do Cell.write x (not c)

  conj = binary \x y z → do
    Cell.lift2 conj x y z

    Cell.watch2 x z \a c → when (a == tt) do Cell.write y c
    Cell.watch2 y z \b c → when (b == tt) do Cell.write x c

  disj = binary \x y z → do
    Cell.lift2 disj x y z

    Cell.watch2 x z \a c → unless (a == tt) do Cell.write y c
    Cell.watch2 y z \b c → unless (b == tt) do Cell.write x c

  not = unary \x y → do
    Cell.lift not x y
    Cell.lift not y x

-- | Any cell can be lifted into a propagator as a nullary propagator (or, as
-- we've known it a couple times, an "emitter"). This lifts the cell into the
-- context, and then applies the `Nullary` constructor.
raise
  ∷ ∀ cell content context
  . MonadCell cell context
  ⇒ cell content
  → Propagator context cell content

raise = Nullary <<< pure

-- | We can also always "lower" the output cell from a propagator by creating a
-- fresh cell, applying the wiring, and then just returning that pre-prepared
-- cell for use. This and `raise` mean we can change our approach depending on
-- the needs we might have, though `Propagator` values are preferred as a more
-- composable way to construct networks.
lower
  ∷ ∀ cell content context
  . MonadCell cell context
  ⇒ Propagator context cell content
  → context (cell content)

lower = case _ of
  Nullary emitter → emitter

  Unary existential → do
    existential # runExists \(Unary_ f cell propagator) → do
      output ← cell
      input  ← lower propagator

      f input output
      pure output

  Binary existential → do
    existential # runExists2 \(Binary_ f cell this that) → do
      c ← cell
      a ← lower this
      b ← lower that

      f a b c
      pure c

-- | We can lift any value into a propagator context by first creating a cell,
-- and then raising that cell into a nullary propagator. This is helpful, for
-- example, when we come to numerical constants.
emit
  ∷ ∀ cell content context
  . JoinSemilattice content
  ⇒ MonadCell cell context
  ⇒ content
  → Propagator context cell content

emit = Nullary <<< Cell.fill

-- | Given a function between propagators, we can "raise" a cell with the input
-- inside and read the output cell (gleaned from lowering the resultant
-- propagator).
forwards
  ∷ ∀ cell context this that
  . JoinSemilattice this
  ⇒ MonadCell cell context
  ⇒ ( Propagator context cell this
    → Propagator context cell that
    )
  → this
  → context that

forwards f value = do
  input  ← Cell.make
  output ← lower (f (raise input))

  Cell.write input value
  Cell.read  output

-- | Far more interestingly, however, we can "raise" a cell with the /output/
-- inside and read the input cell, as long as our computation's relationships
-- are bidirectional (for example, an arithmetic calculation).
backwards
  ∷ ∀ cell context this that
  . JoinSemilattice this
  ⇒ JoinSemilattice that
  ⇒ MonadCell cell context
  ⇒ ( Propagator context cell this
    → Propagator context cell that
    )
  → that
  → context this

backwards f value = do
  input  ← Cell.make
  output ← lower (f (raise input))

  Cell.write output value
  Cell.read  input

-- | Specialising `context` to `ST r` means we can run the computation and
-- remove the context entirely.
forwards_
  ∷ ∀ this that
  . JoinSemilattice this
  ⇒ ( ∀ r
    . Propagator (ST r) (Cell r) this
    → Propagator (ST r) (Cell r) that
    )
  → this
  → that

forwards_ f x = run (forwards f x)

-- | Similarly, `ST r` allows us to write this function that _seems_ to run a
-- function in reverse. In reality, the relationship between "input" and
-- "output" has no real direction, so there's nothing more special about this
-- than `forwards_`. Still, it's a neat party trick.
backwards_
  ∷ ∀ this that
  . JoinSemilattice this
  ⇒ JoinSemilattice that
  ⇒ ( ∀ r
    . Propagator (ST r) (Cell r) this
    → Propagator (ST r) (Cell r) that
    )
  → that
  → this

backwards_ f x = run (backwards f x)

-------------------------------------------------------------------------------

-- | We need a double existential for the binary property, and this is much
-- easier than all the intermediary newtype business that a single existential
-- would require.
foreign import data Exists2 ∷ (Type → Type → Type) → Type

-- | Make a double existential.
mkExists2 ∷ ∀ f a b. f a b → Exists2 f
mkExists2 = unsafeCoerce

-- | Unpack a double existential.
runExists2 ∷ ∀ f r. (∀ a b. f a b → r) → Exists2 f → r
runExists2 = unsafeCoerce


module Data.Propagator
  ( Propagator

  , backwards
  , emit
  , forwards
  , lower
  , raise
  ) where

import Control.Monad.ST.Internal (kind Region)
import Data.Exists (Exists, mkExists, runExists)
import Data.HeytingAlgebra (tt, ff, implies)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Semilattice.Truth as Truth
import Control.Monad.Network (Network, Cell)
import Control.Monad.Network as Network
import Prelude

-- A `Propagator` is a "monotone function between join semilattices". This
-- rather dense statement is explained in more detail in the documentation for
-- `Data.Semilattice.Join`.
--
-- We can take a simpler approach to propagators, however, and use a slightly
-- more hand-wavey definition: a propagator is a function that communicates (or
-- "propagates") knowledge throughout a set of cells. In other words, they are
-- the abstraction by which we share information in our network.
--
-- Following Kmett's formulation, we have three types of propagator that are
-- interesting enough to be constructors in their own right:
--
-- * Nullary. I've coined the term "emitters" for these: effectively, these are
--   outputs without input. Typically, these will indicate constant values
--   (e.g. numeric constants or pre-established facts), and are not influenced
--   by outside information.
--
-- * Unary. Unary propagators are relationships between two cells. If that
--   sounds unfamiliar, consider the unary function `not` as being a
--   relationship between the input and the output. Unary, for our purposes,
--   means "two". _Sorry_. We'd also shy away from using the terms "input" and
--   "output", because data can (and, ideally, does) move in both directions
--   when propagators fire.
--
-- * Binary. Binary propagators are relationships between _three_ cells. Again,
--   think about `(&&)` as a relationship between two inputs and an output.
--   Similar to unary cells, information can travel in all directions: if I
--   know `x + y = z` and I know `y` and `z`, why shouldn't I be able to known
--   `x`?
data Propagator (r ∷ Region) (x ∷ Type)
  = Nullary (Network r (Cell r x))
  | Unary (Exists (Unary_ r x))
  | Binary (Exists (Binary1 r x))

data Unary_ (r ∷ Region) (output ∷ Type) (input ∷ Type)
  = Unary_ (Cell r input → Cell r output → Network r Unit)
           (Network r (Cell r output)) (Propagator r input)

data Binary_ (r ∷ Region) (output ∷ Type) (left ∷ Type) (right ∷ Type)
  = Binary_ (Cell r left → Cell r right → Cell r output → Network r Unit)
            (Network r (Cell r output)) (Propagator r left) (Propagator r right)

newtype Binary1 (r ∷ Region) (output ∷ Type) (left ∷ Type)
  = Binary1 (Exists (Binary_ r output left))

-- Create a unary propagator from a relationship between two cells. These
-- relationships are probably most easily established in conjunction with the
-- `binary` function from `Control.Monad.Network`:
--
-- ```
-- eg0 ∷ ∀ r. Propagator r input → Propagator r output
-- eg0 = unary (Network.unary (_ + Max 5))
-- ```
--
-- ... but, again, we could make this much more useful by adding the inverse:
--
-- ```
-- eg1 ∷ ∀ r. Propagator r input → Propagator r output
-- eg1 = unary \x y → do
--   Network.unary (_ + Max 5) x y
--   Network.unary (_ - Max 5) y x
-- ```
--
-- Now, whenever _one_ of these cells receives new information, so will the
-- other - computation flows in both directions!
unary
  ∷ ∀ r input output
  . JoinSemilattice output
  ⇒ (Cell r input → Cell r output → Network r Unit)
  → Propagator r input
  → Propagator r output
unary go = Unary <<< mkExists <<< Unary_ go Network.make

-- Create a binary propagator from a relationship between three cells. This is
-- easiest to do with the `binary` function from `Control.Monad.Network`; see
-- the above docs for `unary` for examples of usage.
binary
  ∷ ∀ r x y o
  . JoinSemilattice o
  ⇒ (Cell r x → Cell r y → Cell r o → Network r Unit)
  → Propagator r x
  → Propagator r y
  → Propagator r o
binary go this that
  = Binary (mkExists (Binary1 (mkExists (Binary_ go Network.make this that))))

instance semiringPropagator
    ∷ (Eq x, EuclideanRing x, JoinSemilattice x, Show x)
    ⇒ Semiring (Propagator r x) where
  add = binary \x y z → do
    Network.binary (+) x y z
    Network.binary (-) z x y
    Network.binary (-) z y x

  mul = binary \x y z → do
    Network.binary (*) x y z

    let divisor a b
          | b == zero, a /= zero = zero
          | b == zero            = mempty
          | otherwise            = b / a

    Network.binary divisor x z y
    Network.binary divisor y z x

  one  = emit one
  zero = emit zero

instance ringPropagaor
    ∷ ( Show content, Eq content
      , EuclideanRing content
      , JoinSemilattice content
      )
    ⇒ Ring (Propagator r content) where
  sub = binary \x y z → do
    Network.binary (-) x y z
    Network.binary (-) x z y
    Network.binary (+) z y x

instance commutativeRingPropagator
    ∷ ( Show content, Eq content
      , CommutativeRing content
      , EuclideanRing content
      , JoinSemilattice content
      )
    ⇒ CommutativeRing (Propagator r content)

instance divisionRingPropagator
    ∷ ( DivisionRing content
      , Show content, Eq content
      , EuclideanRing content
      , JoinSemilattice content
      )
    ⇒ DivisionRing (Propagator r content) where
  recip = unary \x y → do
    Network.unary recip x y
    Network.unary recip y x

instance euclideanRingPropagator
    ∷ ( Eq content, Show content
      , EuclideanRing content
      , JoinSemilattice content
      )
    ⇒ EuclideanRing (Propagator r content) where
  degree _ = 1

  div = binary \z y x → do
    Network.binary (*) x y z

    let divisor a b
          | b == zero, a /= zero = zero
          | b == zero            = mempty
          | otherwise            = b / a

    Network.binary divisor x z y
    Network.binary divisor y z x

  mod = binary (Network.binary mod)

instance semigroupPropagator
    ∷ (JoinSemilattice content, Show content, Eq content)
    ⇒ Semigroup (Propagator r content) where
  append = binary (Network.binary append)

instance monoidPropagator
    ∷ (JoinSemilattice content, Show content, Eq content)
    ⇒ Monoid (Propagator r content) where
  mempty = emit mempty

instance booleanAlgebraPropagator
    ∷ ( BooleanAlgebra content
      , Show content, Eq content
      , JoinSemilattice content
      )
    ⇒ BooleanAlgebra (Propagator r content)

instance heytingAlgebraPropagator
    ∷ ( Show content, Eq content
      , HeytingAlgebra content
      , JoinSemilattice content
      )
    ⇒ HeytingAlgebra (Propagator r content) where
  ff = emit ff
  tt = emit tt

  implies = binary \x y z → do
    Network.binary implies x y z
    Network.binary (\a c → if  a       == tt then     c else mempty) x z y
    Network.binary (\b c → if (b && c) == tt then not c else mempty) y z x

  conj = binary \x y z → do
    Network.binary conj x y z

    let conj' a b = if a == tt then b else mempty
    Network.binary conj' x z y
    Network.binary conj' y z x

  disj = binary \x y z → do
    Network.binary disj x y z

    let disj' a b = if a == ff then b else mempty
    Network.binary disj' x z y
    Network.binary disj' y z x

  not = unary \x y → do
    Network.unary not x y
    Network.unary not y x

-- Raise a cell into a unary propagator, so that its value can be used within
-- propagator computations. Note that, for nullary propagators, `lower` is the
-- inverse function.
raise ∷ ∀ r content. Cell r content → Propagator r content
raise = Nullary <<< pure

-- Extract an "output cell" from a propagator. While wiring may happen in all
-- directions, the class instances of `Propagator` rely on the "output cell"
-- being the visible type parameter.
--
-- Ideally, the output cell would be constructed within this function, but a
-- lack of GADTs means we can't "prove" `JoinSemilattice` for the existentials
-- in `Unary` or `Binary`.
lower
  ∷ ∀ r content
  . Propagator r content
  → Network r (Cell r content)

lower = case _ of
  Nullary emitter → emitter

  Unary existential → do
    existential # runExists \(Unary_ f cell propagator) → do
      output ← cell
      input  ← lower propagator

      f input output
      pure output

  Binary existential → do
    existential # runExists \(Binary1 existential') →
      existential' # runExists \(Binary_ f cell this that) → do
        c ← cell
        a ← lower this
        b ← lower that

        f a b c
        pure c

-- Create a nullary propagator to "emit" some constant value, initially
-- uninfluenced by the surroundings.
emit
  ∷ ∀ r content
  . JoinSemilattice content
  ⇒ Show content ⇒ Eq content
  ⇒ content
  → Propagator r content

emit value = Nullary do
  fresh ← Network.make
  Network.write fresh (Truth.assert value)

  pure fresh

-- Let's imagine we have a function between propagators like this:
--
-- ```
-- let go ∷ ∀ r. Propagator r (Max Int) → Propagator r (Max Int)
--     go x = x + emit (Max 10)
-- ```
--
-- The `forwards` function allows us to apply this function to a regular,
-- non-propagator argument, in order to produce a regular, non-propagator
-- result.
--
-- ```
-- Network.run (forwards go (Max 5)) -- Max 15
-- ```
--
-- In systems with one user-provided input and a single answer to produce, this
-- can be a nice interface for "hiding" the propagator abstraction from your
-- users.
forwards
  ∷ ∀ r this that
  . JoinSemilattice this
  ⇒ Show this ⇒ Eq this
  ⇒ JoinSemilattice that
  ⇒ Show that ⇒ Eq that
  ⇒ ( ∀ s
    . Propagator s this
    → Propagator s that
    )
  → this
  → Network r that

forwards f value = do
  input  ← Network.make
  output ← lower (f (raise input))

  Network.write input (Truth.assert value)
  Network.read  output

-- If our propagator network's information flows in more than one direction,
-- why limit ourselves to supplying inputs? If we take the same function as we
-- saw with `forwards`, we can use `backwards` to pass it an _output_ and
-- compute an input.
--
-- ```
-- Network.run (backwards go (Max 15)) -- Max 5
-- ```
--
-- Of course, we've seen elsewhere that we describe the contents of cells as
-- _descriptions_ of a value, and so the answer produced by `forwards` and
-- `backwards` may be more general than the one you're expecting, but never in
-- contradiction to it (unless your inputs are not all trustworthy, which may
-- require you to make retractions before obtaining a good answer).
backwards
  ∷ ∀ r this that
  . JoinSemilattice this
  ⇒ Show this ⇒ Eq this
  ⇒ JoinSemilattice that
  ⇒ Show that ⇒ Eq that
  ⇒ ( ∀ s
    . Propagator s this
    → Propagator s that
    )
  → that
  → Network r this

backwards f value = do
  input  ← Network.make
  output ← lower (f (raise input))

  Network.write output (Truth.assert value)
  Network.read  input

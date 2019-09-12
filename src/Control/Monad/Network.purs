module Control.Monad.Network
  ( Cell
  , Network

  , binary
  , identifier
  , make
  , read
  , run
  , unary
  , write
  ) where

import Control.Apply (lift2)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as Ref
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Newtype (class Newtype)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Semilattice.Truth (Truth)
import Data.Semilattice.Truth as Truth
import Data.Set (Set)
import Data.Set as Set
import Prelude

-- A propagator `Network` is a set of `Cell`s (see below for more information),
-- which repeatedly update each other as information is "learned".
--
-- Formally, a propagator is any "monotonic function between join
-- semilattices". In our domain, we can intuit this as, "Any function that
-- spreads knowledge to other cells in our network"
--
-- For more information on semilattices (and what "monotonic" means), see
-- `Data.Semilattice.Join`.
newtype Network (r ∷ Region) (x ∷ Type)
  = Network (StateT { retractions ∷ Set Int, supply ∷ Int } (ST r) x)

derive instance newtypeNetwork ∷ Newtype (Network r x) _
derive newtype instance applicativeNetwork ∷ Applicative (Network r)
derive newtype instance applyNetwork ∷ Apply (Network r)
derive newtype instance bindNetwork ∷ Bind (Network r)
derive newtype instance functorNetwork ∷ Functor (Network r)
derive newtype instance monadNetwork ∷ Monad (Network r)

derive newtype instance monadStateNetwork
  ∷ MonadState { supply ∷ Int, retractions ∷ Set Int } (Network r)

instance semigroupNetwork ∷ Semigroup x ⇒ Semigroup (Network r x) where append = lift2 (<>)
instance monoidNetwork    ∷ Monoid x    ⇒ Monoid    (Network r x) where mempty = pure mempty

-- A cell in our network is a "description of a value". When we create a node,
-- we know "nothing" about its value. As knowledge is propagated around our
-- network, we learn more, and the description becomes more accurate.
--
-- Cells have a unique identifier that we can use to observe how its effect has
-- rippled throughout the network. When the value's description is refined, any
-- cells who may use this information to refine their /own/ descriptions are
-- notified.
newtype Cell (r ∷ Region) (x ∷ Type)
  = Cell
      { identity ∷ Int
      , ref ∷ STRef r
          { truths   ∷ Truth x
          , onChange ∷ Truth x → Network r Unit
          }
      }

-- Make a fresh cell. The cell's description of the inner value is as vague as
-- it can be at this point. For example, if we consider `Max Int` to be the
-- type of a description of a number that is /at least/ as big as our
-- calculated threshold, `minBound` is the "most vague" description. In other
-- words, it's "at least as big as the smallest value possible".
make ∷ ∀ r x. JoinSemilattice x ⇒ Network r (Cell r x)
make = do
  { supply } ← State.get -- Unique identifier.
  _          ← State.modify (_ { supply = supply + 1 })

  contents ← Network $ lift $ Ref.new
    { truths: Truth.assert' (Set.singleton supply) mempty
    , onChange: \_ → pure unit
    }

  pure (Cell { identity: supply, ref: contents })

-- Get a cell's identifier. Every cell has an immutable, unique identifier (a
-- non-negative integer) used primarily for tracking provenance (namely,
-- telling us the origin of the information involved in a particular cell's
-- calculations).
identifier ∷ ∀ r x. Cell r x → Int
identifier (Cell { identity }) = identity

-- Read the current value of a cell. Again, we should see this as a
-- description that can be refined; the value we read will be the "best
-- description of the answer given current information". There's more
-- information about this analogy in `Data.Semilattice.Join`.
read ∷ ∀ r x. JoinSemilattice x ⇒ Cell r x → Network r x
read (Cell { identity, ref }) = do
  { retractions } ← State.get
  { truths }      ← Network (lift (Ref.read ref))

  pure $ truths # foldMapWithIndex \origin value →
    if null (Set.intersection origin retractions) then value else mempty 

-- If one cell's knowledge can be improved by another, the other can be
-- "watched" for changes. Every time its description is refined, the callback
-- will be invoked. Typically, that callback will involve writing to another
-- cell, which is how we propagate information around the network.
watch ∷ ∀ r x. JoinSemilattice x ⇒ Cell r x → (Truth x → Network r Unit) → Network r Unit
watch (Cell { identity, ref }) callback = do
  { onChange, truths } ← Network (lift (Ref.read ref))

  _ ← Network (lift (Ref.modify _ { onChange = onChange *> callback } ref))
  callback truths

-- When we learn something (which we describe as a "truth"), we can write it
-- to a cell. As a result, the description of the cell's value may become
-- further refined. If that happens, all its wachers will be notified, and the
-- learnings will be propagated.
write ∷ ∀ r x. Eq x ⇒ JoinSemilattice x ⇒ Cell r x → Truth x → Network r Unit
write (Cell { identity, ref }) updates = do
  { onChange, truths } ← Network (lift (Ref.read ref))

  let joined = truths <> updates

  when (joined /= truths) do
    _ ← Network (lift (Ref.modify _ { truths = joined } ref))
    onChange joined

-- Any function between join semilattices can be lifted into a relationship
-- between two cells. Specifically, whenever the "input" receives a write, the
-- "output" will be notified of the change.
unary
  ∷ ∀ input output r
  . Eq input  ⇒ JoinSemilattice input
  ⇒ Eq output ⇒ JoinSemilattice output
  ⇒ (input → output)
  → Cell r input → Cell r output
  → Network r Unit
unary f this that = watch this (write that <<< map f)

-- Slightly more complicated is a function from two join semilattices to a
-- third. Here, we update the "output" cell whenever _either_ input is updated.
binary
  ∷ ∀ left right output r
  . Eq left   ⇒ JoinSemilattice left
  ⇒ Eq right  ⇒ JoinSemilattice right
  ⇒ Eq output ⇒ JoinSemilattice output
  ⇒ (left → right → output)
  → Cell r left → Cell r right → Cell r output
  → Network r Unit
binary f left right that = watch left \here → watch right \there → do
  -- TODO? Should this merge logic should live in `Data.Semilattice.Truth`?
  let lefted  = map (\x → f x mempty)  here
      righted = map (\y → f mempty y) there

      learned = here # foldMapWithIndex \old x →
        there # foldMapWithIndex \new y →
          Truth.assert' (old <> new) (f x y)

  write that (lefted <> righted <> learned)

-- Once we have provided all the knowledge we have, we can "run" the
-- computation in order to extract a result.
run ∷ ∀ x. Eq x ⇒ (∀ r. Network r x) → x
run (Network state) = ST.run (evalStateT state initial)
  where initial = { supply: 0, retractions: mempty }

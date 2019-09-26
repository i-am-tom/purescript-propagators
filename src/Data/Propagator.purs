module Data.Propagator
  ( Propagator

  , backwards
  , emit
  , forwards
  , lower
  , raise
  ) where

import Control.Monad.Network (Cell)
import Control.Monad.Network as Network
import Control.Monad.ST (ST)
import Control.Monad.ST.Internal (kind Region)
import Data.Exists (Exists, mkExists, runExists)
import Data.HeytingAlgebra (tt, ff, implies)
import Data.Semilattice.Join (class JoinSemilattice)
import Prelude

data Propagator (r ∷ Region) (x ∷ Type)
  = Nullary (ST r (Cell r x))
  | Unary (Exists (Unary_ r x))
  | Binary (Exists (Binary1 r x))

data Unary_ (r ∷ Region) (output ∷ Type) (input ∷ Type)
  = Unary_ (Cell r input → Cell r output → ST r Unit)
           (ST r (Cell r output)) (Propagator r input)

data Binary_ (r ∷ Region) (output ∷ Type) (left ∷ Type) (right ∷ Type)
  = Binary_ (Cell r left → Cell r right → Cell r output → ST r Unit)
            (ST r (Cell r output)) (Propagator r left) (Propagator r right)

newtype Binary1 (r ∷ Region) (output ∷ Type) (left ∷ Type)
  = Binary1 (Exists (Binary_ r output left))

unary
  ∷ ∀ r x y
  . JoinSemilattice y
  ⇒ (Cell r x → Cell r y → ST r Unit)
  → Propagator r x
  → Propagator r y
unary go = Unary <<< mkExists <<< Unary_ go Network.make

binary
  ∷ ∀ r x y o
  . JoinSemilattice o
  ⇒ JoinSemilattice x
  ⇒ (Cell r x → Cell r y → Cell r o → ST r Unit)
  → Propagator r x
  → Propagator r y
  → Propagator r o
binary go x y
  = Binary  $ mkExists
  $ Binary1 $ mkExists
  $ Binary_ go Network.make x y

instance semiringPropagator
    ∷ (Eq x, JoinSemilattice x, Semiring x, EuclideanRing x)
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
    ∷ (Eq x, EuclideanRing x , JoinSemilattice x)
    ⇒ Ring (Propagator r x) where
  sub = binary \x y z → do
    Network.binary (-) x y z
    Network.binary (-) x z y
    Network.binary (+) z y x

instance commutativeRingPropagator
    ∷ (Eq x, CommutativeRing x, EuclideanRing x, JoinSemilattice x)
    ⇒ CommutativeRing (Propagator r x)

instance divisionRingPropagator
    ∷ (DivisionRing x, Eq x, EuclideanRing x, JoinSemilattice x)
    ⇒ DivisionRing (Propagator r x) where
  recip = unary \x y → do
    Network.unary recip x y
    Network.unary recip y x

instance euclideanRingPropagator
    ∷ (Eq x, EuclideanRing x, JoinSemilattice x)
    ⇒ EuclideanRing (Propagator r x) where
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
    ∷ (JoinSemilattice content, Eq content)
    ⇒ Semigroup (Propagator r content) where
  append = binary (Network.binary append)

instance monoidPropagator
    ∷ (JoinSemilattice content, Eq content)
    ⇒ Monoid (Propagator r content) where
  mempty = emit mempty

instance booleanAlgebraPropagator
    ∷ (BooleanAlgebra content, Eq content, JoinSemilattice content)
    ⇒ BooleanAlgebra (Propagator r content)

instance heytingAlgebraPropagator
    ∷ (Eq content, HeytingAlgebra content, JoinSemilattice content)
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

raise ∷ ∀ r x. Cell r x → Propagator r x
raise = Nullary <<< pure

lower ∷ ∀ r x. Propagator r x → ST r (Cell r x) 
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

emit ∷ ∀ r x. JoinSemilattice x ⇒ Eq x ⇒ x → Propagator r x
emit value = Nullary do
  fresh ← Network.make
  Network.write fresh value

  pure fresh

forwards
  ∷ ∀ r x y
  . JoinSemilattice x ⇒ Eq x
  ⇒ JoinSemilattice y ⇒ Eq y
  ⇒ ( ∀ s
    . Propagator s x
    → Propagator s y
    )
  → x
  → ST r y

forwards f value = do
  input  ← Network.make
  output ← lower (f (raise input))

  Network.write input value
  Network.read  output

backwards
  ∷ ∀ r x y
  . JoinSemilattice x ⇒ Eq x
  ⇒ JoinSemilattice y ⇒ Eq y
  ⇒ ( ∀ s
    . Propagator s x
    → Propagator s y
    )
  → y
  → ST r x

backwards f value = do
  input  ← Network.make
  output ← lower (f (raise input))

  Network.write output value
  Network.read  input

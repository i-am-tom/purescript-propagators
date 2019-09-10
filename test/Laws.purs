module Test.Laws where

import Control.Monad.Rec.Class (tailRec, Step (..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality, cardinality, fromEnum, pred, succ, toEnum)
import Data.Foldable (traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Semilattice.Join (class JoinSemilattice)
import Data.Maybe (Maybe (..))
import Data.Newtype (unwrap)
import Prelude
import Test.QuickCheck ((===), (/==), (>?), (<?), (>=?), (<=?), class Arbitrary)
import Test.QuickCheck.Combinators ((&=&), (|=|), (==>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy)

with ∷ ∀ t. Proxy t → Array (Proxy t → Spec Unit) → Spec Unit
with proxy = traverse_ \law → law proxy

class               (Arbitrary t, Eq t, Show t) ⇐ Testable t
instance testable ∷ (Arbitrary t, Eq t, Show t) ⇒ Testable t

booleanAlgebra ∷ ∀ t. Testable t ⇒ BooleanAlgebra t ⇒ Proxy t → Spec Unit
booleanAlgebra _ = describe "BooleanAlgebra" do
  it "excludedMiddle" $ quickCheck \(x ∷ t) →
    (x || not x) === tt

bounded ∷ ∀ t. Testable t ⇒ Bounded t ⇒ Proxy t → Spec Unit
bounded _ = describe "Bounded" do
  it "ordering" $ quickCheck \(x ∷ t) →
    (bottom <=? x) &=& (x <=? top)

boundedEnum ∷ ∀ t. Testable t ⇒ BoundedEnum t ⇒ Proxy t → Spec Unit
boundedEnum _ = describe "BoundedEnum" do
  let size :: Int
      size = unwrap (cardinality :: Cardinality t)

      count ∷ (t → Maybe t) → t → Int
      count f x = { acc: x, total: 0 } # tailRec \{ acc, total } →
        case f acc of Just y  → Loop { acc: y, total: 1 + total }
                      Nothing → Done total

  it "count succ bottom === cardinality" do
    count succ bottom `shouldEqual` (size - 1)
  
  it "count pred top === cardinality" do
    count pred top `shouldEqual` (size - 1)

  it "succ . pred === id" $ quickCheck \(x ∷ t) →
    (x === bottom) |=| ((pred x >>= succ) === Just x)

  it "pred . succ === id" $ quickCheck \(x ∷ t) →
    (x === top) |=| ((succ x >>= pred) === Just x)

  it "enum / pred commutativity" $ quickCheck \(x ∷ t) →
    (x === bottom) |=| (map fromEnum (pred x) === Just (fromEnum x - 1))

  it "enum / succ commutativity" $ quickCheck \(x ∷ t) →
    (x === top) |=| (map fromEnum (succ x) === Just (fromEnum x + 1))

  it "monotonicity" $ quickCheck \(x ∷ t) (y ∷ t) →
    x `compare` y === fromEnum x `compare` fromEnum y

  it "round-trip" $ quickCheck \(x ∷ t) →
    toEnum (fromEnum x) === Just x

commutativeRing ∷ ∀ t. Testable t ⇒ CommutativeRing t ⇒ Proxy t → Spec Unit
commutativeRing _ = describe "CommutativeRing" do
  it "commutativity" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x * y) === (y * x)

divisionRing ∷ ∀ t. Testable t ⇒ DivisionRing t ⇒ Proxy t → Spec Unit
divisionRing _ = describe "DivisionRing" do
  it "non-zero" do
    (one ∷ t) `shouldNotEqual` (zero ∷ t)

  it "left inverse" $ quickCheck \(x ∷ t) →
    (x /== zero) ==> ((recip x * x) === one)

  it "right inverse" $ quickCheck \(x ∷ t) →
    (x /== zero) ==> ((x * recip x) === one)

enum ∷ ∀ t. Testable t ⇒ Enum t ⇒ Proxy t → Spec Unit
enum _ = describe "Enum" do
  it "successor" $ quickCheck \(x ∷ t) →
    (succ x /== Nothing) ==> (Just x <? succ x)

  it "predecessor" $ quickCheck \(x ∷ t) →
    (pred x /== Nothing) ==> (Just x >? pred x)

  it "pred . succ . pred === pred" $ quickCheck \(x ∷ t) →
    (pred x >>= succ >>= pred) === pred x

  it "succ . pred . succ === succ" $ quickCheck \(x ∷ t) →
    (succ x >>= pred >>= succ) === succ x

  it "non-skipping successor" $ quickCheck \(x ∷ t) (y ∷ t) →
    (y <=? x) |=| (succ x <=? Just y)

  it "non-skipping predecessor" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x <=? y) |=| (Just y <=? pred x)

eq ∷ ∀ t. Testable t ⇒ Proxy t → Spec Unit
eq _ = describe "Eq" do
  it "reflexivity" $ quickCheck \(x ∷ t) →
    (x == x) === true

  it "symmetry" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x == y) === (y == x)

  it "transitivity" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    ((x === y) &=& (y === z)) ==> (x === z)

  it "negation" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x /= y) === not (x == y)

euclideanRing ∷ ∀ t. Testable t ⇒ EuclideanRing t ⇒ Proxy t → Spec Unit
euclideanRing _ = describe "EuclideanRing" do
  it "integral domain" $ quickCheck \(x ∷ t) (y ∷ t) →
    ((x /== zero) &=& (y /== zero)) ==> ((x * y) /== zero)

  it "non-negative Euclidean function" $ quickCheck \(x ∷ t) →
    (x /== zero) ==> (degree x >=? zero)

  it "quotient/remainder" $ quickCheck \(x ∷ t) (y ∷ t) →
    (y /== zero) ==> do
      let q = x / y
          r = x `mod` y

      ((r === zero) |=| (degree r <? degree y))
        &=& (x === (q * y + r))

  it "submultiplicative" $ quickCheck \(x ∷ t) (y ∷ t) →
    ((x /== zero) &=& (y /== zero))
      ==> (degree x <=? degree (x * y))

heytingAlgebra ∷ ∀ t. Testable t ⇒ HeytingAlgebra t ⇒ Proxy t → Spec Unit
heytingAlgebra _ = describe "HeytingAlgebra" do
  it "associativity (&&)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x && (y && z)) === ((x && y) && z)

  it "associativity (||)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x || (y || z)) === ((x || y) || z)

  it "commutativity (&&)" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x && y) === (y && x)

  it "commutativity (||)" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x || y) === (y || x)

  it "absorption (&&)" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x && (x || y)) === x

  it "absorption (||)" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x || (x && y)) === x

  it "idempotence (&&)" $ quickCheck \(x ∷ t) →
    (x && x) === x

  it "idempotence (||)" $ quickCheck \(x ∷ t) →
    (x || x) === x

  it "identity (&&)" $ quickCheck \(x ∷ t) →
    (x && tt) === x

  it "identity (||)" $ quickCheck \(x ∷ t) →
    (x || ff) === x

  it "implication identity" $ quickCheck \(x ∷ t) →
    x `implies` x === tt

  it "implication" $ quickCheck \(x ∷ t) (y ∷ t) →
    ((x && x `implies` y) === (x && y))
      &=& ((y && x `implies` y) === y)

  it "implication distributivity" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x `implies` (y && z)) === ((x `implies` y) && (x `implies` z))

  it "implication complement" $ quickCheck \(x ∷ t) →
    not x === x `implies` ff

joinSemilattice ∷ ∀ t. Testable t ⇒ JoinSemilattice t ⇒ Proxy t → Spec Unit
joinSemilattice _ = describe "JoinSemilattice" do
  it "commutativity" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x <> y) === (y <> x)

  it "idempotence" $ quickCheck \(x ∷ t) (y ∷ t) →
    (x <> y <> y) === (x <> y)

monoid ∷ ∀ t. Testable t ⇒ Monoid t ⇒ Proxy t → Spec Unit
monoid _ = describe "Monoid" do
  it "left identity" $ quickCheck \(x ∷ t) →
    mempty <> x === x

  it "right identity" $ quickCheck \(x ∷ t) →
    x <> mempty === x

ord ∷ ∀ t. Testable t ⇒ Ord t ⇒ Proxy t → Spec Unit
ord _ = describe "Ord" do
  it "reflexivity" $ quickCheck \(x ∷ t) →
    x <=? x

  it "antisymmetry" $ quickCheck \(x ∷ t) (y ∷ t) →
    ((x <=? y) &=& (y <=? x)) ==> (x === y)

  it "transitivity" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    ((x <=? y) &=& (y <=? z)) ==> (x <=? z)

ring ∷ ∀ t. Testable t ⇒ Ring t ⇒ Proxy t → Spec Unit
ring _ = describe "Ring" do
  it "additive inverse" $ quickCheck \(x ∷ t) →
    ((x - x) === zero) &=& ((zero - x) + x === zero)

semigroup ∷ ∀ t. Testable t ⇒ Semigroup t ⇒ Proxy t → Spec Unit
semigroup _ = describe "Semigroup" do
  it "associativity" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x <> y) <> z === x <> (y <> z)

semiring ∷ ∀ t. Testable t ⇒ Semiring t ⇒ Proxy t → Spec Unit
semiring _ = describe "Semiring" do
  it "associativity (+)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    ((x + y) + z) === (x + (y + z))

  it "identity (+)" $ quickCheck \(x ∷ t) →
    (zero + x === x) &=& (x + zero === x)

  it "commutativity (+)" $ quickCheck \(x ∷ t) (y ∷ t) →
    x + y === y + x

  it "associativity (*)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x * y) * z === x * (y * z)

  it "identity (*)" $ quickCheck \(x ∷ t) →
    (one * x === x) &=& (x * one === x)

  it "distributivity (left)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    (x * (y + z)) === ((x * y) + (x * z))

  it "distributivity (right)" $ quickCheck \(x ∷ t) (y ∷ t) (z ∷ t) →
    ((x + y) * z) === ((x * z) + (y * z))

  it "annihilation (left)" $ quickCheck \(x ∷ t) →
    (x * zero) === zero

  it "annihilation (right)" $ quickCheck \(x ∷ t) →
    (zero * x) === zero

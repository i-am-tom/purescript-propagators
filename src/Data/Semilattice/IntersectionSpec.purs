module Data.Semilattice.IntersectionSpec where

import Data.Enum (class BoundedEnum, class Enum, Cardinality (..), fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe (..))
import Data.Semilattice.Intersection (Intersection)
import Prelude
import Test.Laws as Laws
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy (..))

data Sudoku
  = One   | Two   | Three
  | Four  | Five  | Six
  | Seven | Eight | Nine

derive instance eqSudoku      ∷ Eq Sudoku
derive instance ordSudoku     ∷ Ord Sudoku
derive instance genericSudoku ∷ Generic Sudoku _

instance boundedSudoku ∷ Bounded Sudoku where
  top    = Nine
  bottom = One

instance enumSudoku ∷ Enum Sudoku where
  pred x = toEnum (fromEnum x - 1)
  succ x = toEnum (fromEnum x + 1)

instance boundedEnumSudoku ∷ BoundedEnum Sudoku where
  cardinality = Cardinality 9

  fromEnum = case _ of
    One   → 1
    Two   → 2
    Three → 3
    Four  → 4
    Five  → 5
    Six   → 6
    Seven → 7
    Eight → 8
    Nine  → 9

  toEnum = case _ of
    1 → Just One
    2 → Just Two
    3 → Just Three
    4 → Just Four
    5 → Just Five
    6 → Just Six
    7 → Just Seven
    8 → Just Eight
    9 → Just Nine
    _ → Nothing

instance arbitrarySudoku ∷ Arbitrary Sudoku where arbitrary = genericArbitrary
instance showSudoku      ∷ Show      Sudoku where show      = genericShow

spec ∷ Spec Unit
spec = describe "Data.Semilattice.Intersection" do
  Laws.with (Proxy ∷ Proxy (Intersection Sudoku))
    [ Laws.eq
    , Laws.ord
    , Laws.semigroup
    , Laws.monoid
    , Laws.joinSemilattice
    ]

module Test.Main where

import Effect.Aff (launchAff_)
import Effect (Effect)
import Prelude
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  network     ← discover "Control\\.Monad\\.NetworkSpec"
--lattice     ← discover "Data\\.Lattice\\..*Spec"
  propagator  ← discover "Data\\.PropagatorSpec"
  semilattice ← discover "Data\\.Semilattice\\..*Spec"

  runSpec [ consoleReporter ] do
    network
--  lattice
    propagator
    semilattice

module Test.Main where

import Effect.Aff (launchAff_)
import Effect (Effect)
import Prelude
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  lattice    ← discover "Data\\.Lattice\\..*Spec"
  propagator ← discover "Data\\.Propagator\\..*Spec"

  runSpec [ consoleReporter ] do
    lattice
    propagator

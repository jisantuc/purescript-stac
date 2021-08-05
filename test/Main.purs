module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, bind)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_ do
    specs <- discover """Test\..*Spec"""
    runSpec [ consoleReporter ] specs

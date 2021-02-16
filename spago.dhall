{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ps-stac"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "formatters"
  , "psci-support"
  , "quickcheck"
  , "refined"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "stac"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "gen"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "these"
  , "turf"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jisantuc/purescript-stac.git"
}

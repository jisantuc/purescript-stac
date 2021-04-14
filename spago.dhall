{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "stac"
, dependencies =
  [ "aff-promise"
  , "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut"
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
  , "prelude"
  , "psci-support"
  , "quickcheck"
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

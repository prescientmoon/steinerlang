{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "generics-rep"
  , "node-readline"
  , "node-readline-aff"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "stringutils"
  , "undefined"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

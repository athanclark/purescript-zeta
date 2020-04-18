{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "zeta"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foreign-object"
  , "js-timers"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "refs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

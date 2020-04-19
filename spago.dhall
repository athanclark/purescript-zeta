{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "zeta"
, dependencies =
  [ "aff"
  , "arrays"
  , "foreign-object"
  , "refs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/athanclark/purescript-zeta.git"
}

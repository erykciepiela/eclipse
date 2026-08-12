{ name = "eclipse"
, dependencies =
  [ "aff"
  , "arrays"
  , "bambik"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "js-date"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "qualified-do"
  , "record"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , ".spago/bambik/v0.1.5/extras/**/*.purs"
  ]
}

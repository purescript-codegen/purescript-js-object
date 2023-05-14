{ name = "js-object"
, dependencies =
  [ "aff"
  , "contravariant"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "heterogeneous"
  , "js-unsafe-stringify"
  , "newtype"
  , "nullable"
  , "partial"
  , "prelude"
  , "record"
  , "spec"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

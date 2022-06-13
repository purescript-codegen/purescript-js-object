{ name = "js-object"
, dependencies =
  [ "aff", "console", "effect", "prelude", "psci-support", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

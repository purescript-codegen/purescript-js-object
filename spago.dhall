{ name = "js-object"
, dependencies =
      [ "aff", "effect", "heterogeneous", "prelude", "typelevel-prelude" ]
    # [ "console", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

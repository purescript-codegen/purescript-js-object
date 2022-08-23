{ name = "js-object"
, dependencies =
      [ "aff", "effect", "heterogeneous", "prelude", "typelevel-prelude", "contravariant", "newtype", "record", "unsafe-coerce" ]
    # [ "console", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

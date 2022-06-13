let pkgs = ./spago.dhall

in  pkgs
  with dependencies = pkgs.dependencies # [ "spec" ]

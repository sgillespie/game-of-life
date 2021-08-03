{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc884"
}:

let
  pkg = import ./default.nix {
    inherit nixpkgs compiler;
  };

in pkg.env


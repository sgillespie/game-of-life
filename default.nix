{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc884"
}:

let
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
in

haskellPackages.callPackage ./gillespies-game-of-life.nix { }

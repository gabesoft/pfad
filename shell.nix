{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
      monad-par mtl ghci-pretty
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "pfad";
    buildInputs = [ ghc ];
  }

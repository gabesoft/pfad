{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "pfad";
  buildInputs = [ zlib bzip2 haskellPackages.ghci-pretty ];
  inherit ghc;
}

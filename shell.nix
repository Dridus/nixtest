{ nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc }:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "nixtest";
  buildInputs = [ zlib postgresql ];
  inherit ghc;
}



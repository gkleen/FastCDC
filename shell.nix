{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:

pkgs.mkShell {
  name = "fastcdc";
  buildInputs = with haskellPackages; [ ghc stack ];
}

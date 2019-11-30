{ compiler ? "ghc865" }:

let
  bootstrap = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  pkgs = import src {};
  f = import ./default.nix;
  hspkgs = pkgs.haskell.packages.${compiler};
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv

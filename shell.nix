{ pkgs ? import <nixpkgs> {} }:
let

  package = import ./.;

  profilingPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };

  mkEnv = haskellPackages: 
    let derivation = haskellPackages.callPackage package {};
    in  derivation.env.overrideAttrs (oldAttrs: {
      buildInputs = [ pkgs.cabal-install ];
    });

in

  mkEnv pkgs.haskellPackages // {
    withProfiling = mkEnv profilingPackages;
  }

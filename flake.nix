{
  description = "Flake for Migrant multi-package Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        inherit (pkgs) lib;

        # Helper to simplify haskell environment setup
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: { };
        };

        # Create the dev shell with all 4 packages
        devShell = haskellPackages.shellFor {
          packages = p: [
            p.migrant-core
            p.migrant-sqlite-simple
            p.migrant-postgresql-simple
            p.migrant-hdbc
          ];
          withHoogle = true;
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.ghcid
          ];
        };

        # Override package sources to local directories
        migrant-core = haskellPackages.callCabal2nix "migrant-core" ./migrant-core { };
        migrant-sqlite-simple = haskellPackages.callCabal2nix "migrant-sqlite-simple" ./migrant-sqlite-simple { };
        migrant-postgresql-simple = haskellPackages.callCabal2nix "migrant-postgresql-simple" ./migrant-postgresql-simple { };
        migrant-hdbc = haskellPackages.callCabal2nix "migrant-hdbc" ./migrant-hdbc { };
      in {
        devShells.default = devShell;
        packages = {
          inherit migrant-core migrant-sqlite-simple migrant-postgresql-simple migrant-hdbc;
        };
      });
}


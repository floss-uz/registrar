{
  description = "registrar";
  nixConfig = {
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Git hooks
    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        inherit (self.checks.${system}) pre-commit-check;
        # pkgs = import nixpkgs {localSystem = {inherit system;};};
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;
        hpkgs = pkgs.haskell.packages."ghc910".override {
          overrides = self: super: {
            tasty-wai = hlib.dontCheck (hlib.doJailbreak super.tasty-wai);
            servant-client = hlib.dontCheck (hlib.doJailbreak super.servant-client);
            esqueleto = hlib.dontCheck (hlib.doJailbreak super.esqueleto);
            optparse-generic = hlib.dontCheck (hlib.doJailbreak super.optparse-generic);
            postgresql-simple = hlib.dontCheck (hlib.doJailbreak super.postgresql-simple);
            strict-containers = hlib.dontCheck (hlib.doJailbreak super.strict-containers);
          };
        };
      in {
        # Tests and suites for this repo
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              statix.enable = true;
              treefmt.enable = true;

              # When things get nasty
              #flake-checker.enable = true;
            };
          };
        };

        packages.default = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "registrar" ./. {}) (old: {
          doCheck = true;
          doHaddock = false;
          enableLibraryProfiling = false;
          enableExecutableProfiling = false;
        });

        devShells.default = pkgs.callPackage ./shell.nix {inherit pkgs hpkgs pre-commit-hooks pre-commit-check;};
      }
    )
    // {
      hydraJobs = {inherit (self.packages.x86_64-linux) default;};
    };
}

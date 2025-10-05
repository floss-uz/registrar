{
  description = "registrar";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/git-hooks.nix/tarball/master");
        pkgs = import nixpkgs { localSystem = { inherit system; }; };
        hlib = pkgs.haskell.lib;
        hpkgs = pkgs.haskell.packages."ghc912".override {
          overrides = self: super: {
            tasty-wai = hlib.dontCheck (hlib.doJailbreak super.tasty-wai);
            servant-client = hlib.dontCheck (hlib.doJailbreak super.servant-client);
            esqueleto = hlib.dontCheck (hlib.doJailbreak super.esqueleto);
            optparse-generic = hlib.dontCheck (hlib.doJailbreak super.optparse-generic);
          };
        };

        registrar = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "registrar" ./. {}) (old: {
          doCheck = true;
          doHaddock = false;
          enableLibraryProfiling = false;
          enableExecutableProfiling = false;
        });

        pre-commit-check = nix-pre-commit-hooks.run {
          src = ./.;
          # If your hooks are intrusive, avoid running on each commit with a default_states like this:
          # default_stages = ["manual" "pre-push"];
          hooks = {

            # override a package with a different version
            fourmolu.enable = true;
            fourmolu.package = hpkgs.fourmolu;
            fourmolu.settings.defaultExtensions = [ "lhs" "hs" ];

          };
        };
      in
      {
        packages.default = registrar;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            hpkgs.cabal-install
            hpkgs.cabal-add
            hpkgs.haskell-language-server
            hpkgs.fourmolu
            hpkgs.hlint
            hpkgs.hpack
            
            pkgs.just
            pkgs.alejandra
            pkgs.zlib
          ];

          shellHook = ''
            ${pre-commit-check.shellHook}
          '';
        };

        
      }
    );
}

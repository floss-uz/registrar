{
  description = "registrar";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
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

        registrar = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "registrar" ./. { }) (old: {
          doCheck = true;
          doHaddock = false;
          enableLibraryProfiling = false;
          enableExecutableProfiling = false;
        });
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
        };
      }
    );
}


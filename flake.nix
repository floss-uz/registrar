{
  description = "Registrar for keeping everything in one place";

  inputs = {
    # Too old to work with most libraries
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

    # Perfect!
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    # The flake-utils library
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Output results
          binary = pkgs.callPackage ./. { };
          docker = pkgs.dockerTools.streamLayeredImage {
            name = "registrar";
            tag = "latest";
            contents = [ binary ];
            config = {
              Cmd = [ "${binary}/bin/orchestra" ];
            };
          };
        in
        {
          # Nix script formatter
          formatter = pkgs.nixpkgs-fmt;

          # Development environment
          devShells.default = import ./shell.nix { inherit pkgs; };

          # Output package
          packages.default = pkgs.callPackage ./. { };
        }
      )
    // {
      # Overlay module
      # nixosModules.xnux.bot = import ./module.nix self;
    };
}

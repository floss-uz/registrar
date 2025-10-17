{
  pkgs ? let
    lock = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
    nixpkgs = fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${lock.rev}.tar.gz";
      sha256 = lock.narHash;
    };
  in
    import nixpkgs {overlays = [];},
  hpkgs,
  pre-commit-check,
  ...
}:
pkgs.stdenv.mkDerivation {
  name = "registrar";

  # Build time dependencies
  nativeBuildInputs = with pkgs; [
    git
    nixd
    statix
    deadnix
    alejandra
  ];

  # Runtime dependencies
  buildInputs = [
    hpkgs.cabal-install
    hpkgs.cabal-add
    hpkgs.cabal-fmt
    hpkgs.haskell-language-server
    hpkgs.fourmolu
    hpkgs.hlint
    hpkgs.hpack
    hpkgs.postgresql-libpq
    hpkgs.postgresql-libpq-configure

    pkgs.just
    pkgs.alejandra
    pkgs.zlib
    pkgs.treefmt
    pkgs.libpq.dev
    pkgs.zlib.dev
    pkgs.postgresql
    pkgs.libz
    pkgs.libpq.pg_config
    pkgs.pkg-config
    pre-commit-check.enabledPackages
  ];
  # Things to run before entering devShell
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.postgresql}/lib
    ${pre-commit-check.shellHook}
  '';

  # Environmental variables
  NIX_CONFIG = "extra-experimental-features = nix-command flakes";
}

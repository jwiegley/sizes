{
  description = "Compute file sizes";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.sizes.flake {
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            sizes =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc910";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                    ghcid = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                    haskellPackages.fourmolu
                    haskellPackages.hlint
                    lefthook
                  ];
                  withHoogle = true;
                };
              };
          })
        ];

        src = pkgs.lib.cleanSource ./.;

        hsFiles = builtins.concatStringsSep " " [
          "${src}/Sizes.hs"
          "${src}/app/Main.hs"
          "${src}/test/Spec.hs"
        ];

      in flake // {
        packages.default = flake.packages."sizes:exe:sizes";

        checks = (flake.checks or {}) // {
          build = flake.packages."sizes:exe:sizes";

          format = pkgs.runCommand "format-check" {
            nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
          } ''
            fourmolu --mode check ${hsFiles}
            touch $out
          '';

          lint = pkgs.runCommand "lint-check" {
            nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
          } ''
            hlint --hint=${src}/.hlint.yaml ${hsFiles}
            touch $out
          '';
        };
      });
}

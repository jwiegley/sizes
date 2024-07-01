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
                compiler-nix-name = "ghc98";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                    hlint = {};
                    ghcid = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  withHoogle = true;
                };
                # modules = [{
                #   enableLibraryProfiling = true;
                #   enableProfiling = true;
                # }];
              };
          })
        ];
      in flake // {
        packages.default = flake.packages."sizes:exe:sizes";
      });
}

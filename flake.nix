{
  description = "FMCt Programming Language";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };

        # this fixes dynamic linking of glibc on my laptop
        space-ship = with pkgs; writeShellScriptBin "space-ship" ''
          export LD_LIBRARY_PATH=${glibc}/lib
          cabal run space-ship
        '';

        additionalPckgs = with pkgs; [ nixfmt rlwrap space-ship ];


        additionalHaskellPckgs = with pkgs.haskellPackages; [
          structured-haskell-mode
          stylish-haskell

          apply-refact
          cabal-fmt
          cabal-install
          fourmolu
          ghcid
          hasktags
          hlint
          zlib
          haskell-language-server
        ];

        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "space-lang";
            root = self;
            withHoogle = true;
            modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                (additionalHaskellPckgs ++ additionalPckgs);
          };
      in {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}

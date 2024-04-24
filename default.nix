let
  pkgsNix = import ./nix/pkgs.nix;
in
  { pkgs ? pkgsNix.native
  }:

  pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hadolint";
      src = ./.;
    };
    # Specify the GHC version to use.
    compiler-nix-name = "ghc982";
    cabalProjectFreeze = "cabal.project.freeze";
  }

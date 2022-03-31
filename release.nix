let
  pkgsNix = import ./nix/pkgs.nix;

  pkgsNative = pkgsNix.native;
  pkgsx86_64-static = pkgsNix.x86_64-static;
  pkgsAarch64-static = pkgsNix.aarch64-static;
  pkgsAarch64-darwin = pkgsNix.aarch64-darwin;

  hsApp = {
    pkgs ? pkgsNative
  , configureFlags ? []
  }:
    let
      hNix = import ./default.nix { pkgs = pkgs; };
    in
      hNix // {
        hadolint = hNix.hadolint // {
          components = hNix.hadolint.components // {
            exes = hNix.hadolint.components.exes // {
              hadolint = hNix.hadolint.components.exes.hadolint // {
                dontStrip = false;
                dontPatchElf = false;
                configureFlags = configureFlags;
              };
            };
          };
        };
      };

  staticFlags = {pkgs}: [
    "--disable-executable-dynamic"
    "--disable-shared"
    "--ghc-option=-optl=-pthread"
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
  ];

  appNative = hsApp {};

  appx86_64-static = hsApp {
    pkgs = pkgsx86_64-static;
    configureFlags = staticFlags pkgsx86_64-static;
  };

  appAarch64-static = hsApp {
    pkgs = pkgsAarch64-static;
    configureFlags = staticFlags pkgsAarch64-static;
  };

  appAarch64-darwin = hsApp { pkgs = pkgsAarch64-darwin; };
in {
  native = appNative.hadolint.components.exes.hadolint;
  linux-static = appx86_64-static.hadolint.components.exes.hadolint;
  linux-arm-static = appAarch64-static.hadolint.components.exes.hadolint;
  darwin-arm = appAarch64-static.hadolint.components.exes.hadolint;
}

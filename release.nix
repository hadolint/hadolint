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
  ];

  appNative = hsApp {};

  appNativeStatic = hsApp {
    configureFlags = staticFlags pkgsNative;
  };

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
  native-static = appNativeStatic.hadolint.components.exes.hadolint;
  cross-linux-static = appx86_64-static.hadolint.components.exes.hadolint;
  cross-linux-arm-static = appAarch64-static.hadolint.components.exes.hadolint;
  cross-darwin-arm = appAarch64-static.hadolint.components.exes.hadolint;
}

let
  sources = import ./sources.nix {};
  haskellNix = import sources.haskellNix {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

  native = import nixpkgsSrc nixpkgsArgs;

  x86_64-static = import nixpkgsSrc (nixpkgsArgs // {
    crossSystem = native.lib.systems.examples.musl64;
  });

  aarch64-static = import nixpkgsSrc (nixpkgsArgs // {
    crossSystem = native.lib.systems.examples.aarch64-multiplatform-musl;
  });

  aarch64-darwin = import nixpkgsSrc (nixpkgsArgs // {
    crossSystem = native.lib.systems.examples.aarch64-darwin;
  });

in {
  inherit haskellNix;

  inherit nixpkgsSrc nixpkgsArgs;

  inherit native x86_64-static aarch64-static aarch64-darwin;
}

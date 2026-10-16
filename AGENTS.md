# Working on Hadolint

## Build and check

Run commands from the repository root. Native development uses GHC and
`cabal-install`. Use `.github/workflows/haskell.yml` for the native CI compiler
and `devenv.nix` for the Nix development environment; these need not use the same
compiler. Keep dependency bounds and component declarations in `hadolint.cabal`,
and consult `cabal.project` for project-specific overrides. Initial setup is
described in `README.md` under **Develop / Setup**.

For native Haskell changes, the CI build and test sequence is:

```sh
cabal check
cabal configure --enable-tests
cabal build
cabal test all --test-show-details=direct
```

For a focused rule test, replace `DL3000` with the relevant Hspec description
and confirm that the intended examples actually ran:

```sh
cabal test hadolint-unit-tests --test-show-details=direct \
  --test-options='--match DL3000'
```

Use `cabal run exe:hadolint -- --help` to invoke the checkout's CLI rather than an
unrelated installed binary. For Haskell linting, run `hlint src/ test/ app/`;
`.github/workflows/hlint.yml` defines the CI scope and failure threshold.
`devenv.nix` enables `ormolu` and `cabal-fmt` formatting hooks.
Preserve nearby formatting and keep changes focused; follow
`.remarkrc.yaml` for Markdown.

## Rules and their tests

Dockerfile parsing and AST definitions belong to the `language-docker` dependency.
For parser changes or testing a local dependency checkout, read **AST** and
**Building against custom libraries** in `README.md` before changing Hadolint's
rule logic.

When adding a Hadolint rule:

- Add its implementation under `src/Hadolint/Rule/`, following the `DLxxxx.hs`
  naming convention. In `src/Hadolint/Rule.hs`, a `simpleRule` predicate returns
  `True` for valid instructions and `False` to emit a diagnostic; use
  `customRule` or `veryCustomRule` when state or end-of-file decisions are needed.
- Import and compose the rule in `failures` in `src/Hadolint/Process.hs`.
  Merely adding the source file does not enable the rule.
- Add the library module to `exposed-modules` and its test module to the
  `hadolint-unit-tests` suite's `other-modules` in `hadolint.cabal`.
- Add `test/Hadolint/Rule/DLxxxxSpec.hs`, exporting `spec`.
  `test/Spec.hs` uses `hspec-discover`; follow the existing discovery convention.
- Update the **Rules** table in `README.md` and add the rule to
  `contrib/hadolint.json`.

Use the small inline Dockerfiles and `ruleCatches` / `ruleCatchesNot` helpers shown
in `test/Hadolint/Rule/DL3000Spec.hs`, including its implicit `?config` binding.
The helpers live in `test/Helpers.hs`. `ruleCatches` accepts one or more findings;
use `assertChecks` with `failsWith` when the exact diagnostic count matters.
For rules that apply inside `ONBUILD`, consult `onbuild` in `src/Hadolint/Rule.hs`
and the `onBuildRuleCatches` / `onBuildRuleCatchesNot` helpers.

The rule helpers call `Hadolint.Process.run`, not `Hadolint.Lint.analyze`.
Pragma handling is in the former, while production severity overrides and
configuration-wide ignored-rule filtering are applied by the latter in
`src/Hadolint/Lint.hs`. Tests of those later steps must exercise that path;
a passing rule-helper test alone does not cover it. CLI configuration assembly
and exit-status decisions are in `app/Main.hs`.

For configuration changes, inspect the field-specific merge semantics of
`PartialConfiguration` in `src/Hadolint/Config/Configuration.hs`.
In `src/Hadolint/Config/Commandline.hs`, absent boolean configuration flags
must remain `Nothing`, not `Just False`, to avoid overriding other sources.

## Native and WebAssembly boundaries

`hadolint.cabal` selects `app/Main.hs` for the native `hadolint` executable and
`app/embedded.hs` for `hadolint-embedded` on `wasm32`. Native tests do not validate
the embedded build. For embedded or website work, consult the `wasm32` sections
of `hadolint.cabal` and `cabal.project`, and the toolchain and build steps in
`.github/workflows/build.yml`.

`website/hadolint.wasm` and `website/js/ghc_wasm_jsffi.js` are outputs of that
workflow, not sources to hand-edit. The workflow also deploys GitHub Pages;
do not trigger it merely to validate a local change.

## Scripts with broader effects

`integration_test.sh` is an unmaintained legacy corpus scan: it deletes
and recreates `tests/`, clones many external repositories, invokes `stack install`,
and suppresses individual lint failures. Do not use it as a substitute for the
Cabal tests above. The unit-test sources are in `test/`, not `tests/`.

The `prepare-release`, `release`, and `do-release` scripts in `devenv.nix` modify
versions or Git state and can publish packages. They are release operations,
not build or test shortcuts.
For release automation and autotagging, consult `.github/workflows/release.yml`.

For pull requests, follow `.github/PULL_REQUEST_TEMPLATE.md` and report the
checks actually run, including any validation gaps.

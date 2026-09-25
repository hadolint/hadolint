# Repository Guidelines

## Dos and Don'ts

- **Do** load the repo environment with `direnv allow` or `devenv shell` so `cabal-install`, `ormolu`, and `cabal-fmt` match the setup encoded in `.envrc` and `devenv.nix`.
- **Do** mirror CI by running `cabal configure --enable-tests`, `cabal build`, and `cabal test all --test-show-details=direct` from the repo root before sending a change (see `.github/workflows/haskell.yml`).
- **Do** keep rule changes and tests paired: update `src/Hadolint/Rule/*.hs` and add matching specs under `test/Hadolint/Rule/*Spec.hs`.
- **Don't** run `./integration_test.sh` unless the network-heavy cloning and `stack install` dependency are acceptable; it pulls dozens of external repositories into `tests/`.
- **Don't** mass-reformat or rename modules—follow the existing layout and whitespace so diffs stay minimal.
- **Don't** edit release automation (`devenv.nix` scripts, `.github/workflows/*`) without coordinating, because they drive multi-platform builds and publishing.

## Project Structure and Module Organization

- `hadolint.cabal`, `cabal.project`: Cabal targets, exposed modules, and dependency bounds for the library, executable, and test suite.
- `app/Main.hs`: CLI entry point wiring configuration, lint execution, and exit codes.
- `src/Hadolint/`: Core library modules (config parsing, formatters, rules, shell helpers); each rule lives in `Hadolint/Rule/DLxxxx.hs`.
- `test/`: Hspec suite discovered via `test/Spec.hs`; rule specs mirror the library modules and share helpers in `test/Helpers.hs`.
- `contrib/hadolint.json`: JSON schema for configuration consumers.
- `docker/`: Dockerfile and notes for container builds.
- `docs/INTEGRATION.md`, `docs/RELEASE.md`: Integration guidance and release process.
- `.github/workflows/`: CI definitions for testing, linting (HLint), releases, and third-party notices.
- `scripts/`: Utility scripts such as `scripts/fetch_version.sh` used in packaging flows.

## Build, Test, and Development Commands

- Use `direnv allow` (or `devenv shell`) once per clone to populate the toolchain declared in `devenv.nix`; this installs `cabal`, `ghc`, and helper scripts locally.
- Fetch the package index with `cabal update` before the first build or after changing dependencies.
- Configure and build with tests enabled using `cabal configure --enable-tests` followed by `cabal build`.
- Run the whole suite exactly as CI does: `cabal test all --test-show-details=direct` (the GitHub workflow also sets `HSPEC_OPTIONS=--color`).
- Scope tests when iterating: `cabal test hadolint-unit-tests --test-options '--match "DL3001"'` filters to matching Hspec examples.
- Explore modules interactively with `cabal repl` and the `:set -XOverloadedStrings` command highlighted in `README.md`.
- Run linting locally if `hlint` is installed: `hlint src app test` matches the CI paths in `.github/workflows/hlint.yml`.
- Optional integration sweep (slow, networked): `./integration_test.sh` clones reference Dockerfiles and runs the compiled binary via `stack install`—use only when you need broad regression coverage.

## Coding Style and Naming Conventions

- Haskell formatting follows `.hindent.yaml` (4-space indent, 100-character lines) and the existing layout; avoid introducing trailing whitespace or reflowing long string literals.
- Keep modules and tests in camel-cased namespaces (`Hadolint.Rule.DL3050`, `Hadolint.Rule.DL3050Spec`) so they align with the Cabal listings.
- Organize imports exactly as in surrounding files—standard imports first, then qualified ones, preserving alphabetical order and blank-line grouping.
- Prefer single-line `--` comments; multi-line blocks use `{- -}` sparingly and match current style.
- Format Cabal files with `cabal-fmt --inplace hadolint.cabal` after edits (the command is provided via `devenv.nix`).
- Markdown and docs respect `.remarkrc.yaml` (100-character limit, `_` markers for emphasis); run remark only when necessary to avoid large diffs.

## Testing Guidelines

- Unit tests live in `test/` and use Hspec with discovery via `test/Spec.hs`; each rule has a matching `Hadolint.Rule.*Spec` module.
- Update `hadolint.cabal`'s `other-modules` list when you add new specs so the suite compiles.
- Run `cabal test all --test-show-details=direct` before pushing; add `--test-options '--fail-fast'` locally if you need quicker feedback.
- Use targeted runs during development with `cabal test hadolint-unit-tests --test-options '--match "Rule DL3050"'` to focus on specific examples.
- Treat `./integration_test.sh` as optional regression coverage; it requires network access and `stack` and writes to `tests/`.

## Commit and Pull Request Guidelines

- Write imperative, capitalized commit subjects with a blank line before the body and wrap lines at ~72 characters (e.g., `Handle RPM package names with plus signs`).
- Reference issues in commits or PR descriptions using `fixes #123` or `closes #123` when applicable, matching `.github/PULL_REQUEST_TEMPLATE.md` prompts.
- Keep commits focused on one concern and ensure `cabal build`, `cabal test`, and `hlint` pass locally before requesting review.
- Include screenshots or terminal captures in PRs whenever CLI output or formatting changes affect users.
- Merge-ready diffs should already include updated tests, docs, and Cabal metadata where required.

## Safety and Permissions

- **Allowed**: list or read files, run targeted `cabal build`/`cabal test`/`hlint` on touched modules, edit code under `src/`, `app/`, `test/`, and adjust docs closely related to the change.
- **Ask first**: add or upgrade dependencies, modify `.github/workflows/*`, run the `./integration_test.sh` sweep, or change release scripts in `devenv.nix`/`scripts/`.
- **Never**: delete release tags or binaries, rewrite published history, or perform destructive operations outside the repository workspace.

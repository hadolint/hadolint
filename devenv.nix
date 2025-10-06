{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.git
    pkgs.cabal-install
    pkgs.haskellPackages.cabal-fmt
  ];

  # https://devenv.sh/languages/
  languages.haskell.enable = true;

  enterShell = ''
    ghc --version
  '';

  # https://devenv.sh/tasks/
  tasks = {
    "language-docker:build".exec = "cabal build";
    "language-docker:docs".exec = "cabal haddock";
    "language-docker:test".exec = "cabal test";
    "language-docker:check".exec = "cabal check";
  };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
    ghc --version | grep --color=auto "${pkgs.ghc.version}"
  '';

  # https://devenv.sh/git-hooks/
  git-hooks.hooks.cabal-fmt.enable = true;
  git-hooks.hooks.ormolu.enable = true;

  # https://devenv.sh/scripts/
  scripts.bump-patch-version.exec = ''
    set -ex
    # in hadolint.cabal, the version is defined as
    # version: x.x.x # major.minor.patch format (e.g. 1.2.3)
    # use bash to increment the patch version by one
    current_version=$(grep '^version:' hadolint.cabal | awk '{print $2}')
    IFS='.' 
    read -r major minor patch <<< "$current_version"
    new_patch=$((patch + 1))
    new_version="$major.$minor.$new_patch"
    sed -i "s/^version: .*/version: $new_version/" hadolint.cabal
  '';

  scripts.bump-minor-version.exec = ''
    set -ex
    current_version=$(grep '^version:' hadolint.cabal | awk '{print $2}')
    IFS='.'
    read -r major minor patch <<< "$current_version"
    new_minor=$((minor + 1))
    new_version="$major.$new_minor.0"
    sed -i "s/^version: .*/version: $new_version/" hadolint.cabal
  '';

  scripts.bump-major-version.exec = ''
    set -ex
    current_version=$(grep '^version:' hadolint.cabal | awk '{print $2}')
    IFS='.'
    read -r major minor patch <<< "$current_version"
    new_major=$((major + 1))
    new_version="$new_major.0.0"
    sed -i "s/^version: .*/version: $new_version/" hadolint.cabal
  '';

  scripts.prepare-release.exec = ''
    set -ex
    git pull

    if [ -z "$BUMP" ]; then
      echo "BUMP environment variable is not set. Exiting."
      exit 1
    fi

    case "$BUMP" in
      patch)
      bump-patch-version
      ;;
      minor)
      bump-minor-version
      ;;
      major)
      bump-major-version
      ;;
      *)
      echo "Invalid BUMP value. Use 'patch', 'minor', or 'major'."
      exit 1
      ;;
    esac

    cabal-fmt --inplace hadolint.cabal

    cabal test
    cabal check
    cabal haddock
  '';

  scripts.release.exec = ''
    set -ex
    git add hadolint.cabal
    git commit -m "Bump version"
    git tag -a "v$(grep '^version:' hadolint.cabal | awk '{print $2}')" -m "v$(grep '^version:' hadolint.cabal | awk '{print $2}')"
    git push origin master --tags

    rm -f dist-newstyle/sdist/*.tar.gz
    cabal sdist
    cabal upload --publish dist-newstyle/sdist/*.tar.gz
    rm -f dist-newstyle/sdist/*.tar.gz
  '';

  scripts.do-release.exec = ''
    set -ex
    prepare-release
    release
  '';

  # See full reference at https://devenv.sh/reference/options/
}

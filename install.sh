#!/bin/sh
# Install the hadolint binary from a GitHub release.
#
#   curl -sSL https://raw.githubusercontent.com/hadolint/hadolint/refs/heads/master/install.sh | sh
#   gh release download --repo hadolint/hadolint --pattern 'install.sh' --output - | sh
#
# Pass options through `sh -s --`, e.g. `... | sh -s -- -v v2.12.0`.
#
# Options:
#   -v VERSION   release tag to install (default: latest, env HADOLINT_VERSION)
#   -b DIR       installation directory (env HADOLINT_INSTALL_DIR); defaults to
#                /usr/local/bin if writable, otherwise $HOME/.local/bin
#   -h           show this help
#
# The binary is verified against the release's checksums.sha256 before it is
# installed.

set -eu

REPO="hadolint/hadolint"
VERSION="${HADOLINT_VERSION:-latest}"
INSTALL_DIR="${HADOLINT_INSTALL_DIR:-}"

usage() {
  sed -n '2,/^$/{s/^# \{0,1\}//;/^$/d;p;}' "$0" 2>/dev/null || echo "usage: install.sh [-v VERSION] [-b DIR]"
}

log() { printf '%s\n' "$*" >&2; }
die() { log "error: $*"; exit 1; }

while getopts "v:b:h" opt; do
  case "$opt" in
    v) VERSION="$OPTARG" ;;
    b) INSTALL_DIR="$OPTARG" ;;
    h) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done

detect_platform() {
  os="$(uname -s)"
  arch="$(uname -m)"
  case "$os" in
    Linux) os=linux ;;
    Darwin) os=macos ;;
    *) die "unsupported operating system: $os (see https://github.com/$REPO/releases)" ;;
  esac
  case "$arch" in
    x86_64 | amd64) arch=x86_64 ;;
    arm64 | aarch64) arch=arm64 ;;
    *) die "unsupported architecture: $arch" ;;
  esac
  # Release assets: hadolint-{linux,macos}-{x86_64,arm64}
  ASSET="hadolint-$os-$arch"
}

fetch() { # url dest
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL -o "$2" "$1"
  elif command -v wget >/dev/null 2>&1; then
    wget -q -O "$2" "$1"
  else
    die "either curl or wget is required"
  fi
}

sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d ' ' -f 1
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | cut -d ' ' -f 1
  else
    die "sha256sum or shasum is required to verify the download"
  fi
}

default_install_dir() {
  if [ -w /usr/local/bin ]; then
    echo /usr/local/bin
  else
    echo "$HOME/.local/bin"
  fi
}

main() {
  detect_platform

  if [ -n "${HADOLINT_BASE_URL:-}" ]; then
    base="$HADOLINT_BASE_URL" # for testing
  elif [ "$VERSION" = latest ]; then
    base="https://github.com/$REPO/releases/latest/download"
  else
    base="https://github.com/$REPO/releases/download/$VERSION"
  fi

  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT INT TERM

  log "Downloading $ASSET ($VERSION)..."
  fetch "$base/$ASSET" "$tmp/$ASSET" || die "could not download $base/$ASSET"
  fetch "$base/checksums.sha256" "$tmp/checksums.sha256" || die "could not download checksums.sha256"

  # Lines look like "<hash> *<name>" (binary mode) or "<hash>  <name>".
  expected="$(awk -v n="$ASSET" '{ f = $2; sub(/^\*/, "", f); if (f == n) print $1 }' "$tmp/checksums.sha256")"
  [ -n "$expected" ] || die "no checksum for $ASSET in checksums.sha256"
  actual="$(sha256_of "$tmp/$ASSET")"
  [ "$expected" = "$actual" ] || die "checksum mismatch for $ASSET (expected $expected, got $actual)"
  log "Checksum OK."

  [ -n "$INSTALL_DIR" ] || INSTALL_DIR="$(default_install_dir)"
  chmod +x "$tmp/$ASSET"
  if mkdir -p "$INSTALL_DIR" 2>/dev/null && [ -w "$INSTALL_DIR" ]; then
    mv "$tmp/$ASSET" "$INSTALL_DIR/hadolint"
  elif command -v sudo >/dev/null 2>&1; then
    log "Installing to $INSTALL_DIR with sudo..."
    sudo mkdir -p "$INSTALL_DIR"
    sudo mv "$tmp/$ASSET" "$INSTALL_DIR/hadolint"
  else
    die "cannot write to $INSTALL_DIR"
  fi

  log "Installed hadolint to $INSTALL_DIR/hadolint"
  case ":$PATH:" in
    *":$INSTALL_DIR:"*) ;;
    *) log "Note: $INSTALL_DIR is not in your PATH." ;;
  esac
}

main

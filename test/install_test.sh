#!/bin/sh
# Tests install.sh against a fake release served from file:// URLs.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

case "$(uname -s)" in Linux) os=linux ;; Darwin) os=macos ;; *) echo "skip"; exit 0 ;; esac
case "$(uname -m)" in x86_64 | amd64) arch=x86_64 ;; *) arch=arm64 ;; esac
asset="hadolint-$os-$arch"

rel="$tmp/release"
mkdir -p "$rel"
printf '#!/bin/sh\necho fake-hadolint\n' > "$rel/$asset"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$rel" && sha256sum -b "$asset" > checksums.sha256)
else
  (cd "$rel" && shasum -a 256 -b "$asset" > checksums.sha256)
fi

export HADOLINT_BASE_URL="file://$rel"

# 1. valid checksum installs a working binary
sh "$ROOT/install.sh" -b "$tmp/bin"
[ "$("$tmp/bin/hadolint")" = fake-hadolint ] || { echo "FAIL: binary not installed"; exit 1; }

# 2. tampered binary is rejected and nothing is installed
printf 'tampered\n' >> "$rel/$asset"
if sh "$ROOT/install.sh" -b "$tmp/bin2" 2>/dev/null; then
  echo "FAIL: checksum mismatch was accepted"; exit 1
fi
[ ! -e "$tmp/bin2/hadolint" ] || { echo "FAIL: unverified binary installed"; exit 1; }

echo "install.sh tests passed"

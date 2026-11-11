#!/bin/sh
# Tests install.sh against a fake release served from file:// URLs.
set -eu

SH="$(command -v sh)"
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

# 2. without -b, installs into the first writable PATH directory
mkdir -p "$tmp/ro" "$tmp/rw1" "$tmp/rw2"
chmod 555 "$tmp/ro"
PATH="/nonexistent:relative:$tmp/ro:$tmp/rw1:$tmp/rw2:$PATH" sh "$ROOT/install.sh" 2>/dev/null
[ -x "$tmp/rw1/hadolint" ] && [ ! -e "$tmp/rw2/hadolint" ] || { echo "FAIL: PATH dir selection"; exit 1; }
chmod 755 "$tmp/ro"

# 3. no writable PATH directory is an error
mkdir "$tmp/tools"
for t in uname mktemp awk cut chmod rm mkdir mv sed cat curl wget sha256sum shasum perl env bash; do
  p=""
  for c in "/usr/bin/$t" "/bin/$t" "$(command -v "$t" 2>/dev/null || true)"; do
    [ -x "$c" ] && { p="$c"; break; }
  done
  [ -n "$p" ] && ln -s "$p" "$tmp/tools/$t"
done
chmod 555 "$tmp/tools"
if PATH="$tmp/tools" "$SH" "$ROOT/install.sh" 2>"$tmp/err"; then
  echo "FAIL: expected failure without writable PATH dir"; exit 1
fi
grep -q "no writable directory" "$tmp/err" || { echo "FAIL: wrong error"; cat "$tmp/err"; exit 1; }
chmod 755 "$tmp/tools"

# 4. tampered binary is rejected and nothing is installed
printf 'tampered\n' >> "$rel/$asset"
if sh "$ROOT/install.sh" -b "$tmp/bin2" 2>/dev/null; then
  echo "FAIL: checksum mismatch was accepted"; exit 1
fi
[ ! -e "$tmp/bin2/hadolint" ] || { echo "FAIL: unverified binary installed"; exit 1; }

echo "install.sh tests passed"

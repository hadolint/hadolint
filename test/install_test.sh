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

# Read-only directory with just the tools install.sh needs, so PATH can be
# controlled precisely.
mkdir "$tmp/tools"
for t in uname tr mktemp awk cut chmod rm mkdir mv sed cat curl wget sha256sum shasum perl env bash; do
  p=""
  for c in "/usr/bin/$t" "/bin/$t" "$(command -v "$t" 2>/dev/null || true)"; do
    [ -x "$c" ] && { p="$c"; break; }
  done
  [ -n "$p" ] && ln -s "$p" "$tmp/tools/$t"
done
chmod 555 "$tmp/tools"

# 2. without -b, installs into the first default directory that is in PATH and
# writable, ignoring other writable PATH directories
home="$tmp/home"
mkdir -p "$home/.local/bin" "$home/bin" "$tmp/other"
HOME="$home" PATH="$tmp/other:$home/bin:$home/.local/bin:$tmp/tools" "$SH" "$ROOT/install.sh" 2>/dev/null
[ -x "$home/.local/bin/hadolint" ] || { echo "FAIL: default dir selection"; exit 1; }
[ ! -e "$tmp/other/hadolint" ] || { echo "FAIL: non-default PATH dir used"; exit 1; }

# 2b. a default directory that is not in PATH is skipped
rm "$home/.local/bin/hadolint"
HOME="$home" PATH="$home/bin:$tmp/tools" "$SH" "$ROOT/install.sh" 2>/dev/null
[ -x "$home/bin/hadolint" ] && [ ! -e "$home/.local/bin/hadolint" ] || { echo "FAIL: not-in-PATH dir used"; exit 1; }

# 3. no usable default directory is an error
if PATH="$tmp/tools" "$SH" "$ROOT/install.sh" 2>"$tmp/err"; then
  echo "FAIL: expected failure without writable PATH dir"; exit 1
fi
grep -q "is a writable directory in PATH" "$tmp/err" || { echo "FAIL: wrong error"; cat "$tmp/err"; exit 1; }
chmod 755 "$tmp/tools"

# 4. tampered binary is rejected and nothing is installed
printf 'tampered\n' >> "$rel/$asset"
if sh "$ROOT/install.sh" -b "$tmp/bin2" 2>/dev/null; then
  echo "FAIL: checksum mismatch was accepted"; exit 1
fi
[ ! -e "$tmp/bin2/hadolint" ] || { echo "FAIL: unverified binary installed"; exit 1; }

echo "install.sh tests passed"

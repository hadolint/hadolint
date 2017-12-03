#!/usr/bin/env bash

# In case of shallow clone fetch repo tags and 100 commits to get full
# `git description` message for `hadolint --version`

set -evuo pipefail

# Get remote URL which can have two formats
# git@github.com:hadolint/hadolint.git
# https://github.com/hadolint/hadolint

url=$(git remote get-url origin)
# if URL is for SSH, change it to HTTPS format
if [[ $url =~ "git@" ]]; then
  url=https://github.com/$(cut -f 2 -d ":" <<< "$url")
fi

git fetch --tags "$url"
git fetch --depth=100 "$url"

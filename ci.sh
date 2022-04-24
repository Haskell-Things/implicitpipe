#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"

echo "regenerating .github/workflows/ci.yaml..."

mkdir -p .github/workflows

# based on https://github.com/vmchale/github-actions-dhall
which dhall-to-yaml || cabal install dhall-yaml
dhall-to-yaml --file ci.dhall > .github/workflows/ci.yaml

#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

here="$(dirname "$0")"
cd "$here/.."

cabal test common-concrete-syntax || icdiff common-concrete-syntax/test/cases/allLexemes.{golden,output}

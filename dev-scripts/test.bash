#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

here="$(dirname "$0")"
cd "$here/.."

cabal build common-concrete-syntax-test
# cabal test common-concrete-syntax || icdiff common-concrete-syntax/test/cases/allTokens.{golden,output}
cabal test common-concrete-syntax || \
  # icdiff common-concrete-syntax/test/cases/allTokens.{golden,output}
  # icdiff common-concrete-syntax/test/cases/all{Raw,}Lexemes.output
  icdiff common-concrete-syntax/test/cases/allLexemes.{golden,output}

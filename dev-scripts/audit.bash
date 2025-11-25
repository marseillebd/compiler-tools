#/usr/bin/env bash
set -euo pipefail
shopt -s globstar

here="$(dirname "$0")"
cd "$here/.."

re() {
  git ls-files \
  | grep -v '^\.' \
  | xargs grep -r \
    --color=always \
    -I \
    -s \
    --exclude="audit.bash" \
    --exclude=Raw.hs \
    --exclude=MyLib.hs \
    $@
}

# look for outstanding issues listed in the code
echo ====== ISSUES IN SOURCE ======
re -F FIXME || true
re -F DEBUG || true
re -F TODO || true
re -F DELETE || true
re -F NOTE || true

# look for unsafe functions
echo ====== UNSAFE FUNCTIONS ======
re -P '(undefined|unsafe|OrPanic|die)' || true
re -P '(?<=^|[=($]|->)\s*error(WithoutStackTrace)?\b' || true

# look for extra ghc extensions
echo ====== LANGUAGE EXTENSIONS ======
re -F LANGUAGE || true

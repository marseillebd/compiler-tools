#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

here="$(dirname "$0")"
cd "$here/.."

if [[ "$(date +%k)" -lt 6 ]]; then
  today="$(date -I -d yesterday)"
else
  today="$(date -I)"
fi

f="NOTES/logbook/${today}.md"
if [[ ! -e "$f" ]]; then
  touch "$f"
  git add "$f"
fi
exec $EDITOR "$f"

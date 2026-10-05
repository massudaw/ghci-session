#!/bin/sh
# This directory is where ghci-session is developed; github.com/massudaw/ghci-session is a copy of it.
# Copy the tree as it is committed here (HEAD) into a checkout of that repository and commit it there.
#   ghci-session/backport.sh [CHECKOUT]     (default ../../ghci-session, i.e. ~/code/ghci-session)
# It does not push: look at the commit, then `git -C CHECKOUT push`.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
TO=$(cd "${1:-$HERE/../../ghci-session}" && pwd)
[ -d "$TO/.git" ] || { echo "backport: $TO is not a git checkout" >&2; exit 1; }
[ -z "$(git -C "$TO" status --porcelain)" ] || { echo "backport: $TO has uncommitted changes" >&2; exit 1; }
REV=$(git -C "$HERE" rev-parse --short HEAD)
git -C "$TO" rm -rqf . >/dev/null
git -C "$HERE" archive HEAD . | tar -x -C "$TO"
git -C "$TO" add -A
if [ -z "$(git -C "$TO" status --porcelain)" ]; then echo "backport: nothing new since the last one"; exit 0; fi
git -C "$TO" commit -q -m "${BACKPORT_MESSAGE:-Backport from sprinkler-solver $REV}"
git -C "$TO" show --stat --oneline HEAD | tail -15
echo "backport: committed in $TO (not pushed)"

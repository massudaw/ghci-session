#!/bin/sh
# A project's Claude Code (or Codex) conversations read for their facts into a store of its own, with nothing of
# the project touched: its sessions are not restarted, its histories not written, and your real store of facts
# (~/.local/state/ghci-session/knowledge) is not changed until you choose to.
#
#   tools/know-trial.sh PROJECT-DIR [--since YYYY-MM-DD] [--all]
#
# A throwaway project named as PROJECT-DIR is (so its facts are filed under that name) gets a session with
# "knowledge": true and no summaries (GHS_KNOWLEDGE_ONLY=1: a history brought in only for its facts is not paid
# for twice), the conversations are imported into it, and the daemon reads them: one model call a message of the
# user's or the agent's, and one for each few facts to set them against what is known. Left in .bin/know-trial/:
# store/ (look: GHS_KNOWLEDGE=.bin/know-trial/store ghci-session knowledge) and the session's usage.jsonl.
# To keep the facts: append store/facts.jsonl to the real store's, then `ghci-session knowledge refile -n`.
set -e
HERE=$(cd "$(dirname "$0")/.." && pwd)
G=$HERE/.bin/ghci-session
[ -n "$1" ] || { sed -n 2,14p "$0"; exit 2; }
SRC=$(cd "$1" && pwd); shift
NAME=$(basename "$SRC")
T=$HERE/.bin/know-trial
P=$T/$NAME
CLAUDE=$HOME/.claude/projects/$(printf %s "$SRC" | sed 's/[^A-Za-z0-9]/-/g')
[ -d "$CLAUDE" ] || { echo "know-trial: no Claude Code conversations for $SRC ($CLAUDE)" >&2; exit 1; }
mkdir -p "$P/src" "$T/store"
cat > "$P/demo.cabal" <<EOC
cabal-version: 2.4
name:          demo
version:       0.1
library
  hs-source-dirs:   src
  exposed-modules:  Demo
  build-depends:    base
  default-language: Haskell2010
EOC
echo "packages: ." > "$P/cabal.project"
printf 'module Demo (greeting) where\n\ngreeting :: String\ngreeting = "hello"\n' > "$P/src/Demo.hs"
cat > "$P/ghci-session.json" <<EOC
{"default": "demo", "hygiene": false, "knowledge": true, "summarize_jobs": 4,
 "summarize_cmd": "GHS_PROVIDER=claude GHS_CLAUDE_SUMMARIZE_MODEL=claude-haiku-5-5 ghci-session summarize",
 "targets": {"demo": {"units": ["lib:demo"], "watch": ["src"], "modules": ["Demo"]}}}
EOC
cd "$P"
export GHS_KNOWLEDGE=$T/store GHS_KNOWLEDGE_ONLY=1
"$G" start demo | tail -1
"$G" import --claude "$CLAUDE" "$@" | tail -4
printf 'import these? [y/N] '; read a; [ "$a" = y ] || { "$G" stop demo >/dev/null; exit 0; }
"$G" import --claude "$CLAUDE" "$@" --go | tail -1
S=.ghci-session/demo
while :; do
  u=$(python3 -c "import json;print(json.load(open('$S/history/know.json'))['upto'])" 2>/dev/null || echo 0)
  n=$(cat $S/history/main/*.jsonl | wc -l)
  printf '\r%s of %s messages read' "$u" "$n"
  [ "$u" -ge "$n" ] && break
  sleep 5
done
echo
"$G" stop demo >/dev/null
"$G" knowledge

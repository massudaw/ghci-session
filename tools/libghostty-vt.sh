#!/bin/sh
# Build libghostty-vt -- Ghostty's terminal emulation as a C library, what `ghci-session top` runs its panes
# on -- and put it beside the executable (.bin/libghostty-vt.so, or .dylib on macOS). Needs git and zig 0.16
# (downloaded here when there is none on PATH), a few minutes and ~1 GB of build cache. The checkout is kept
# under .bin/ghostty-vt-build/ for the next run.
#
#   tools/libghostty-vt.sh              # the commit the vendored headers came from (ghostty-vt/COMMIT)
#   tools/libghostty-vt.sh main         # any ref
set -e
HERE=$(cd "$(dirname "$0")/.." && pwd)
REF=${1:-$(cat "$HERE/ghostty-vt/COMMIT")}
B="$HERE/.bin/ghostty-vt-build"
mkdir -p "$B"
cd "$B"
if ! command -v zig >/dev/null 2>&1 || ! zig version | grep -q '^0\.16\.'; then
  if [ ! -x "$B/zig/zig" ]; then
    case "$(uname -s)-$(uname -m)" in
      Linux-x86_64) T=zig-x86_64-linux-0.16.0 ;;
      Linux-aarch64) T=zig-aarch64-linux-0.16.0 ;;
      Darwin-arm64) T=zig-aarch64-macos-0.16.0 ;;
      Darwin-x86_64) T=zig-x86_64-macos-0.16.0 ;;
      *) echo "no zig 0.16 download known for $(uname -s)-$(uname -m): install zig 0.16 and run again" >&2; exit 1 ;;
    esac
    echo "downloading zig 0.16.0 ..." >&2
    curl -sSL -o zig.tar.xz "https://ziglang.org/download/0.16.0/$T.tar.xz"
    tar xJf zig.tar.xz && rm -f zig.tar.xz && mv "$T" zig
  fi
  PATH="$B/zig:$PATH"
fi
if [ ! -d ghostty/.git ]; then
  git clone -q --filter=blob:none https://github.com/ghostty-org/ghostty ghostty
fi
cd ghostty
git fetch -q --depth 1 origin "$REF" 2>/dev/null || git fetch -q origin
git checkout -q "$REF" 2>/dev/null || git checkout -q FETCH_HEAD
echo "building libghostty-vt at $(git rev-parse --short HEAD) ..." >&2
zig build -Demit-lib-vt -Doptimize=ReleaseFast
case "$(uname -s)" in
  Darwin) cp "$(ls zig-out/lib/libghostty-vt.*.dylib zig-out/lib/libghostty-vt.dylib 2>/dev/null | head -1)" "$HERE/.bin/libghostty-vt.dylib" ;;
  *) cp "$(ls zig-out/lib/libghostty-vt.so.* | head -1)" "$HERE/.bin/libghostty-vt.so" ;;
esac
ls "$HERE/.bin"/libghostty-vt.* >&2

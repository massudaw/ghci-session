#!/bin/bash
# `cabal repl --with-repl=` wrapper giving GHCi its OWN RTS flags (GHS_RTS_FLAGS, e.g. "-c").
# `--repl-options=+RTS` reaches the compiled program's RTS, not GHCi's, and GHCRTS reaches cabal too,
# which is not linked with -rtsopts and refuses to start; a top-level `ghc +RTS ... -RTS` is the only way in.
# -T enables GHC.Stats, which the session's memory report reads.
exec ghc +RTS ${GHS_RTS_FLAGS:--c} -T -RTS "$@"

#!/bin/bash
# `cabal repl --with-repl=` target for the vendored engine: our GHCi (GHS_ENGINE), told where the compiler's
# libraries are (the real `ghc` is a script that does the same) and given GHCi's RTS flags.
exec "$GHS_ENGINE" -B"$GHS_LIBDIR" +RTS ${GHS_RTS_FLAGS:--c} -RTS "$@"

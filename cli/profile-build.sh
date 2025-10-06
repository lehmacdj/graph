#!/usr/bin/env bash
stack build \
  --profile \
  --ghc-options="-O -fno-prof-auto -fprof-late -fprof-cafs" \
  --ghc-options="-finfo-table-map -fdistinct-constructor-tables" \
  --ghc-options="-ddump-simpl -ddump-to-file" \
  # --ghc-options="-ticky -ticky-allocd -ticky-dyn-thunk"

# RTS option presets
# (pass after `--` in `stack --profile exec` or `stack --profile test`):
# - memory by type + json format allocations/time info:
#   `+RTS -l-agu -p -pj -hy -po <custom name> -RTS`
# other useful RTS options:
# -lT: include ticky info in eventlog
# -hy: heap profile by type
# -hm: heap profile by module
# -hc: heap profile by cost center
# -hi: profile by info table (gives source locations)
# -N1 -hb: biographical profiling, can help find wasted space i.e. drag/void

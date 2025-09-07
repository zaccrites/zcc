#!/usr/bin/env bash

set -e
set -x

cabal run
gcc out.s -o out

# We expect this to "fail", at least as long as its only way to
# emit any output is through its exit code.
set +e
./out
echo "$?"


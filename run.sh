#!/usr/bin/env bash

set -e
cabal run
gcc out.s -o out
gcc -O0 sample.c -o sample

# We expect this to "fail", at least as long as its only way to
# emit any output is through its exit code.
set +e
./out
echo "zcc: $?"

./sample
echo "gcc: $?"


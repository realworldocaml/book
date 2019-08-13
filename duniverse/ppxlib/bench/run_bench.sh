#!/bin/bash

set -e -o pipefail

ROOT="$(hg root)"
ppx="$ROOT/.ppx/ppx_jane/ppx.exe"

time $ppx -dump-ast -inline-test-lib blah -o /dev/null input.ml

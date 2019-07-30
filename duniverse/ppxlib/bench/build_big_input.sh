#!/bin/bash

set -e -o pipefail

ROOT="$(hg root)"

OUT=input.ml
echo > input.ml

find $ROOT/lib/{core,core_kernel,async,async_{kernel,unix,extra}}/src -name \*.ml | {
    while read fn; do
        if ! grep -q '^#' "$fn"; then
            cat >> input.ml <<EOF

# 2 "$fn"

EOF
            cat "$fn" >> input.ml
        fi
    done
}

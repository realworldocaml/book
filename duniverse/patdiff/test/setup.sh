#!/bin/bash

export HERE=$(readlink -f $(dirname "$BASH_SOURCE"))
export PATH="$(readlink -f $HERE/../bin/):$PATH"

function patdiff {
    patdiff.exe "$@"
}
export -f patdiff

function visible_colors {
    $HERE/../../ansicodes/bin/main.exe visualize -minimize
}

export -f visible_colors

function start_test {
    set -u -o pipefail
}

export -f start_test

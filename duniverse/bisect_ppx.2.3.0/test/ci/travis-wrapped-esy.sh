#!/usr/bin/env bash

if [ "$1" == install ]
then
    EXTRA=--skip-repository-update
else
    EXTRA=
fi

exec $HOME/build/aantron/bisect_ppx/node_modules/.bin/esy "$@" $EXTRA

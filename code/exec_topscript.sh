#!/usr/bin/env bash

topscript=`pwd`/../scripts/_build/run_core_toplevel.byte
cd $(dirname $1)
$(topscript) $(basename $1)

#!/bin/sh

set -e

if [ $# -ne 2 ]; then
    echo "Usage: discover.sh OCAMLC OUTFILE" >&2
    exit 2
fi

OCAMLC="$1"
OUTFILE="$2"
shift 2

OCAML_CFLAGS=
. ./setup.data
[ "$posix_timers" = true ] && OCAML_CFLAGS="$OCAML_CFLAGS -ccopt -DPOSIX_TIMERS"

SRC=config/test.c
OUT=config/result.h
trap "rm -f $OUT" EXIT

sentinel="CORE_`basename "$OUTFILE" | tr a-z. A-Z_`"
cat > $OUT  <<EOF
#ifndef $sentinel
#define $sentinel
EOF

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee >> $OUT

cat >> $OUT  <<EOF
#endif
EOF

mv $OUT $OUTFILE

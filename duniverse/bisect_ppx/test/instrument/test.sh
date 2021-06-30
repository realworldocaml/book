#!/usr/bin/env bash

echo "(lang dune 2.7)" > dune-project

echo "(executable" > dune
echo " (name test)" >> dune
echo " (modes byte)" >> dune
echo " (ocamlc_flags -dsource)" >> dune
echo " (instrumentation (backend bisect_ppx)))" >> dune

echo > .ocamlformat

rm -f test.ml
while read line
do
  echo "$line" >> test.ml
done < /dev/stdin

sanitize() {
  # [@@@ocaml.text "/*"] is the delimiter in the output. Bisect_ppx runtime
  # registration code begins at the first line containing that text. The
  # instrumented module proper begins after the second such line.

  THRESHOLD=${1:-2}
  COUNT=0

  while read line
  do
    if [ $COUNT -ge $THRESHOLD ]
    then
      echo "$line"
    fi

    if [ "$line" == "[@@@ocaml.text \"/*\"]" ]
    then
      COUNT=$(($COUNT + 1))
    fi
  done
}

if [ "$1" == "--include-registration" ]
then
  DELIMITERS=1
fi

dune build ./test.bc --instrument-with bisect_ppx 2>&1 \
| sanitize $DELIMITERS \
| ocamlformat --name test.ml -

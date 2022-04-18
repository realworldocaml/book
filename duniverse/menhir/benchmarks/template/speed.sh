#!/bin/bash

# Outputs and measurements are stored in the directory times/.
rm -rf times && mkdir times

# The sentences.
SENTENCES=$(cd sentences && ls *.tokens)

# The backends.
BACKENDS=$(cd backends && ls -d *.backend)

# Loop over all input sentences.
for file in $SENTENCES ; do
  base=${file%.tokens}
  # Loop over all backends.
  for backend in $BACKENDS ; do
    backend=${backend%.backend}
    # echo "$base [$backend]..."
    backends/"$backend".backend/main.exe sentences/"$file" \
       > times/"$base"."$backend".out \
      2> times/"$base"."$backend".time
  done
done

# The base name of the current directory is the name of the parser
# that we are using in this benchmark.
XXX=$(basename "$(pwd)")

# Collect the results.
./speed.exe "$XXX"

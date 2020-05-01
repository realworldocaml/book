#!/bin/bash
set -euo pipefail

# This script promotes a specific test, named on the command line.

# To promote a test means to create or update its expected-output files.

# Examples:
#   ./promote.sh good/mezzo
#   ./promote.sh bad/option

# If this is a newly created test, then [make depend] should be run first
# for dune to know about this test.

for name in "$@"
do
  if [[ $name =~ ^good/.* ]] ; then
    # A positive test.
    echo "Promoting $name..."
    # Create the expected-output files if they are missing.
    touch test/static/$name.opp.exp
    touch test/static/$name.exp
    base=${name#good/}
    # Ask dune to update the expected-output files.
    dune build @$base --auto-promote
  elif [[ $name =~ ^bad/.* ]] ; then
    # A negative test.
    echo "Promoting $name..."
    # Create the expected-output file if it is missing.
    touch test/static/$name.exp
    base=${name#bad/}
    # Ask dune to update the expected-output file.
    dune build @$base --auto-promote
  else
    # Unrecognized.
    echo "Don't know what to do with '$name'."
    echo "This script handles tests whose name begins with good/ or bad/."
    exit 1
  fi
done

#!/bin/bash
set -euo pipefail

# This script re-runs a specific test, named on the command line.

# Examples:
#   ./exec.sh good/mezzo
#   ./exec.sh bad/option

# If this is a newly created test, then [make depend] should be run first
# for dune to know about this test.

for name in "$@"
do
  if [[ $name =~ ^good/.* ]] ; then
    # A positive test.
    base=${name#good/}
    rm -f _build/default/test/static/"$name".out
    dune build @$base
    # Display the timings.
    cat _build/default/test/static/"$name".timings
  elif [[ $name =~ ^bad/.* ]] ; then
    # A negative test.
    base=${name#bad/}
    rm -f _build/default/test/static/"$name".out
    dune build @$base
    # Display the output.
    cat _build/default/test/static/"$name".out
  else
    # Unrecognized.
    echo "Don't know what to do with '$name'."
    echo "This script handles tests whose name begins with good/ or bad/."
    exit 1
  fi
done

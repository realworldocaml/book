#! /bin/bash

# ./corpus.sh scans all .cmti and .cmt, and optionally .cmi files, in the
# current opam switch. For each one, it runs odoc compile, which triggers the
# odoc parser on every comment found. If any errors or warnings are reported by
# the parser, they will be written to stderr.
#
# A corpus of .cmt[i] files is typically installed with something like
#
#   opam install lwt --criteria='+count(solution)'
#
#
# Typical usage is somehing like:
#
# ./corpus.sh > results.txt 2>&1

SWITCH=$(opam config var prefix)

CMTI=$(find $SWITCH/lib -name '*.cmti')
CMT=$(find $SWITCH/lib -name '*.cmt')
# CMI=$(find $SWITCH/lib -name '*.cmi')

mkdir -p _build/

function apply_odoc
{
    FILES=$1

    for FILE in $FILES
    do
        echo $FILE
        odoc compile --package corpus -o _build/file.odoc $FILE 2>&1
    done
}

apply_odoc "$CMTI"
apply_odoc "$CMT"
# apply_odoc "$CMI"

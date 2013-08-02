#!/bin/sh

for file in "$@"; do
   sed -f clean.sed < $file > $file.tmp
   mv $file.tmp $file
done

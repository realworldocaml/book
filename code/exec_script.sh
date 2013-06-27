#!/usr/bin/env bash

cd $(dirname $1)
rm -rf _build
while read line; do
  echo "$ $line"
  $line
done < $(basename $1)

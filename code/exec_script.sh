#!/usr/bin/env bash
cd $(dirname $1)
while read line; do
  echo "$ $line"
  $line
done < $(basename $1)

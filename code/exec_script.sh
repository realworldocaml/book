#!/usr/bin/env bash
cd $(dirname $1)
while IFS= read -r line ; do
  echo "$ $line"
  bash -c "$line"
done < $(basename $1)

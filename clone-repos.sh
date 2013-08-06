#!/bin/sh -x

if [ ! -d examples/.git ]; then
  if [ -d examples ]; then
    mv examples examples.old-binaries
  fi
  git clone git://github.com/realworldocaml/examples
  if [ -d code ]; then
    mv code code.old-binaries
  fi
  ln -nfs examples/code code
fi

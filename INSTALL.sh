#!/bin/sh

opam install --yes \
  core \
  core_extended \
  cryptokit \
  core_bench \
  atdgen \
  async \
  yojson \
  textwrap \
  cohttp \
  async_graphics \
  menhir 

echo You also need Pygments installed (python-pygments in Debian)

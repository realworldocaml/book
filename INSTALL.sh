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
  menhir \
  utop

echo You also need Pygments installed.
echo This is python-pygments in Debian

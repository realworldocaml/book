#!/bin/sh

opam install -j 4 --yes \
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
  utop \
  cmdliner \
  cow \
  ocp-indent \
  ctypes \
  ocp-index

echo You also need Pygments installed.
echo This is python-pygments in Debian

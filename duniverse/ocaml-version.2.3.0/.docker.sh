#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine}
VERSIONS=${OCAML_VERSIONS:-4.04 4.05 4.06 4.07}

set -ex
case $DISTRO in
alpine-*) sudo apk add m4 ;;
debian-*) sudo apt -y install m4 pkg-config ;;
ubuntu-*) sudo apt -y install m4 pkg-config ;;
esac

sudo chown -R opam /home/opam/src
cd /home/opam/src
export OPAMYES=1
export OPAMJOBS=3
opam install --deps-only .
rm -f jbuild-workspace.dev
echo "(lang dune 1.0)" > jbuild-workspace.dev
for v in $VERSIONS; do
  echo "(context (opam (switch $v)))" >> jbuild-workspace.dev
  opam install --deps-only -t --switch $v .
done

dune build --workspace jbuild-workspace.dev
rm -f jbuild-workspace.dev

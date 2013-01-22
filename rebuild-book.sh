#!/usr/bin/env bash
set -xe

REPO=git@github.com:yminsky/OCaml-Book
git pull -u $REPO
make clean
make trunk
./gen-pdf.sh
cp book.pdf data/live_site/trunk/rwo-snapshot.pdf
cp buildlog.txt data/live_site/trunk/buildlog.txt

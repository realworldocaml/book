#!/usr/bin/env bash
set -xe

REPO=git@github.com:yminsky/OCaml-Book
LOCAL=~/src/git/avsm/OCaml-Book
HTML=$LOCAL/build/en/html
SVNR=~/src/svn/current
cd $LOCAL
git pull -u $REPO
make clean
make
make oreilly
REVID=`git rev-parse HEAD`
cp build/en/source/rwo-oreilly.xml $SVNR/book.xml
cd $SVNR
svn commit -m 'autocommit from $REPO $REVID orm:commitpdf'
sleep 10
cp pdf/book.xml.pdf $HTML/rwo-snapshot.pdf

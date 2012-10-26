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
cd $SVNR
svn update
cp $LOCAL/build/en/source/rwo-oreilly.xml $SVNR/book.xml
svn commit -m 'autocommit from $REPO $REVID orm:commitpdf'
sleep 60
svn update
cp pdf/book.xml.pdf $HTML/rwo-snapshot.pdf
cp pdf/.buildlog $HTML/buildlog.txt


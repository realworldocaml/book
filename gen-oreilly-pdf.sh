#!/usr/bin/env bash
set -xe

SVNR=~/svn/current
CURDIR=`pwd`
cd $SVNR
svn update
cp ${CURDIR}/build/en/source/rwo-oreilly.xml $SVNR/book.xml
svn commit -m "autocommit from $REPO orm:commitpdf"
sleep 30
svn update
cp pdf/book.xml.pdf ${CURDIR}/data/live_site/trunk/rwo-snapshot.pdf
cp pdf/.buildlog ${CURDIR}/data/live_site/trunk/buildlog.txt


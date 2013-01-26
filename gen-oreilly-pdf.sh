#!/usr/bin/env bash
set -xe

SVNR=~/svn/current
CURDIR=`pwd`
cd $SVNR
svn update
cp ${CURDIR}/build/en/source/rwo-oreilly.xml $SVNR/book.xml
svn commit -m "autocommit from $REPO orm:commitpdf"
sleep 60
svn update
cp pdf/book.xml.pdf ${CURDIR}/book.pdf
cp pdf/.buildlog ${CURDIR}/buildlog.txt
cat buildlog.txt

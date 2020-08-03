#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This script checks that a Menhir tarball can be compiled and installed.
# The command line argument should be the tarball's name without .tar.gz.

PACKAGE="$1"
TARBALL=$PACKAGE.tar.gz

# We use a dedicated opam switch where it is permitted to uninstall/reinstall
# Menhir.

if opam switch list | grep 'test-menhir' >/dev/null ; then
  echo "The switch test-menhir already exists." ;
else
  echo "Creating switch test-menhir..." ;
  opam switch create test-menhir 4.10.0 ;
  echo "Installing required packages..." ;
  opam install --yes dune visitors coq ;
fi

echo "Now switching to test-menhir..."
eval $(opam env --set-switch --switch test-menhir)

# Uninstall Menhir if it is installed.

echo "Removing menhir if already installed..."
#   read -p "Can I remove it [Enter/^C]?" -n 1 -r ;
opam remove menhir || /bin/true

# Create a temporary directory; extract into it.
# Build and install; then uninstall.

TEMPDIR=`mktemp -d /tmp/menhir-test.XXXXXX`
INSTALL=$TEMPDIR/install
COQCONTRIB=$INSTALL/coq-contrib
DUNE=dune

cp $TARBALL $TEMPDIR

echo "   * Extracting. "
(cd $TEMPDIR && tar xfz $TARBALL)

echo "   * Compiling and installing."
mkdir $INSTALL
(cd $TEMPDIR/$PACKAGE &&
  $DUNE build @install &&
  $DUNE install --prefix=$INSTALL menhir &&
  make -C coq-menhirlib all &&
  make -C coq-menhirlib CONTRIB=$COQCONTRIB install
) > $TEMPDIR/install.log 2>&1 || (cat $TEMPDIR/install.log; exit 1)

echo "   * Uninstalling."
(cd $TEMPDIR/$PACKAGE &&
  $DUNE uninstall --prefix=$INSTALL menhir
  make -C coq-menhirlib CONTRIB=$COQCONTRIB uninstall
) > $TEMPDIR/uninstall.log 2>&1 || (cat $TEMPDIR/uninstall.log; exit 1)

rm -rf $TEMPDIR

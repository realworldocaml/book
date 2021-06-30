#!/bin/bash
set -euo pipefail
IFS=$' \n\t'

# This script compiles OCaml using the current version of Menhir and verifies
# that it behaves exactly as expected.

MENHIR_ROOT=$(pwd)

# We use a dedicated opam switch where it is permitted to uninstall/reinstall
# Menhir.

if opam switch list | grep 'test-menhir' >/dev/null ; then
  echo "The switch test-menhir already exists." ;
else
  echo "Creating switch test-menhir..." ;
  opam switch create test-menhir ocaml-system.4.11.1 ;
  echo "Installing required packages..." ;
  opam install --yes dune ;
fi

eval $(opam env --set-switch --switch test-menhir)

# Uninstall Menhir if it is installed.

# We are not sure whether it was installed via opam or directly
# via [make install], so we try both uninstallation methods.

echo "Removing menhir if already installed..."
#   read -p "Can I remove it [Enter/^C]?" -n 1 -r ;
(opam remove menhir menhirLib menhirSdk || /bin/true) >/dev/null 2>&1
make -C $MENHIR_ROOT uninstall >/dev/null 2>&1

# Check if everything has been committed.
# This seems required for [opam pin] to work properly.

if git status --porcelain | grep -v compile-ocaml ; then
  echo "Error: there remain uncommitted changes." ;
  git status ;
  exit 1 ;
  fi

# This function runs a command silently, and prints its execution time.

execute () {
  echo "$1" > .command
  T="$(date +%s)"
  if eval "$1" >log.out 2>log.err ; then
    T="$(($(date +%s)-T))"
    echo " $T seconds." ;
  else
    code=$?
    echo " failure."
    cat log.err
    exit $code
  fi
}

# Check out a fresh copy of OCaml at a specific tag.

TEMPDIR=/tmp/menhir-test-ocaml
mkdir -p $TEMPDIR
cd $TEMPDIR
rm -rf ocaml

echo -n "Cloning OCaml..."
execute "git clone git@github.com:ocaml/ocaml.git --depth 1 --branch 4.11.1"

cd ocaml

# Configure and compile OCaml. This step does not depend on Menhir.

echo -n "Configuring OCaml..."
execute "./configure"

echo -n "Compiling OCaml..."
execute "make -j"
ls -l ocamlc

if false ; then
  echo -n "Testing OCaml..."
  execute "make -C testsuite parallel"
  execute "make -C testsuite clean"
fi

# Install Menhir.

# Should we install Menhir via [make install] or via [opam]?

# [make install], which invokes [dune install], is likely to be much
# faster (especially if Menhir has already been compiled in its
# working directory). Unfortunately, I have seen dune (2.8.2) become
# confused and install files partly in one switch, partly in another,
# so perhaps installing via [opam pin ...] is preferable.

echo -n "Installing Menhir..."

if true ; then
  # Installation via opam.
  execute "make -C $MENHIR_ROOT pin"
else
  # Installation via [make install].
  execute "make -C $MENHIR_ROOT install"
fi
ls -l `which menhir`

# Re-compile OCaml's parser using Menhir.

echo -n "Recompiling OCaml's parser using Menhir..."
execute "make promote-menhir"

echo -n "Committing the recompiled parser..."
execute "git add boot/menhir && git commit -m 'make promote-menhir'"

# Take a snapshot of the ASTs produced by the current parser.

echo -n "Constructing ASTs for all source files..."
execute "make -j build-all-asts"

echo -n "Committing all ASTs..."
execute "make list-all-asts | xargs git add && git commit -m 'Build all ASTs.'"

# Compile OCaml (again).

# Cleaning up first should be unnecessary, but let's make sure the
# compiler is correctly reconstructed from scratch.

echo -n "Cleaning up..."
execute "make clean"

echo -n "Compiling OCaml..."
execute "make -j"
ls -l ocamlc

if false ; then
  echo -n "Testing OCaml..."
  execute "make -C testsuite parallel"
fi

# Reconstruct all ASTs.

# Removing them first should be unnecessary, but let's make sure they
# are correctly reconstructed from scratch.

echo -n "Removing previous ASTs..."
execute "make list-all-asts | xargs rm -f"

echo -n "Constructing ASTs for all source files..."
execute "make -j build-all-asts"

# Compare the ASTs produced by the current parser with the snapshot.

rm -f .command log.{err,out}

if git diff --exit-code >/dev/null ; then
  echo "Success: the original parser and the recompiled parser agree."
else
  echo "Failure: the original parser and the recompiled parser disagree."
  echo "cd $TEMPDIR/ocaml && git status"
fi

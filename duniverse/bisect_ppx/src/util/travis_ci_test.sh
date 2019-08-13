#!/usr/bin/env bash

set -e
set -x

travis_install_on_linux () {
    wget https://github.com/ocaml/opam/releases/download/2.0.0/opam-2.0.0-x86_64-linux
    sudo mv opam-2.0.0-x86_64-linux /usr/local/bin/opam
    sudo chmod a+x /usr/local/bin/opam

    sudo apt-get install -qq time git

    case "$OCAML_VERSION" in
        4.02)
            opam init -y --disable-sandboxing --compiler=4.02.3 ;;
        4.03)
            opam init -y --disable-sandboxing --compiler=4.03.0 ;;
        4.04)
            opam init -y --disable-sandboxing --compiler=4.04.2 ;;
        4.05)
            opam init -y --disable-sandboxing --compiler=4.05.0 ;;
        4.06)
            opam init -y --disable-sandboxing --compiler=4.06.1 ;;
        4.07)
            opam init -y --disable-sandboxing --compiler=4.07.1 ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

travis_install_on_osx () {
    brew update > /dev/null
    # See https://github.com/Homebrew/homebrew-core/issues/26358.
    brew upgrade python > /dev/null
    brew install opam

    case "$OCAML_VERSION" in
        4.02)
            opam init -y --disable-sandboxing --compiler=4.02.3 ;;
        4.03)
            opam init -y --disable-sandboxing --compiler=4.03.0 ;;
        4.04)
            opam init -y --disable-sandboxing --compiler=4.04.2 ;;
        4.05)
            opam init -y --disable-sandboxing --compiler=4.05.0 ;;
        4.06)
            opam init -y --disable-sandboxing --compiler=4.06.1 ;;
        4.07)
            opam init -y --disable-sandboxing --compiler=4.07.1 ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

case $TRAVIS_OS_NAME in
    osx) travis_install_on_osx ;;
    linux) travis_install_on_linux ;;
    *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# Prepare environment
eval `opam config env`

# Check packages
ocaml -version | grep $OCAML_VERSION
opam --version
git --version

echo
echo "Install dependencies"
echo
opam install -y ocamlfind ocamlbuild ocaml-migrate-parsetree ppx_tools_versioned

echo
echo "Compiling"
echo
make build

opam install -y ounit
# opam install -y ppx_blob ppx_deriving

echo
echo "Testing"
echo
make test

echo
echo "Testing package usage and Ocamlbuild plugin"
echo
make usage

echo
echo "Testing installation"
echo
make clean
opam pin add -yn bisect_ppx .
opam install -y bisect_ppx
ocamlfind query bisect_ppx bisect_ppx.runtime
which bisect-ppx-report
opam pin add -yn bisect_ppx-ocamlbuild .
opam install -y bisect_ppx-ocamlbuild
ocamlfind query bisect_ppx-ocamlbuild

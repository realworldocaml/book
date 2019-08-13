set -x



# Install system packages.
packages_apt () {
    wget https://github.com/ocaml/opam/releases/download/2.0.0/opam-2.0.0-x86_64-linux
    sudo mv opam-2.0.0-x86_64-linux /usr/local/bin/opam
    sudo chmod a+x /usr/local/bin/opam

    if [ "$LIBEV" != no ]
    then
        sudo apt-get update -qq
        sudo apt-get install -qq libev-dev
    fi
}

packages_osx () {
    brew update > /dev/null
    # See https://github.com/Homebrew/homebrew-core/issues/26358.
    brew upgrade python > /dev/null
    brew install opam

    if [ "$LIBEV" != no ]
    then
        brew install libev
    fi
}

packages () {
    case $TRAVIS_OS_NAME in
        linux) packages_apt;;
          osx) packages_osx;;
            *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
    esac
}

packages



# Initialize opam.
opam init -y --bare --disable-sandboxing --disable-shell-hook
opam switch create . $COMPILER $REPOSITORIES --no-install
eval `opam env`
opam --version
ocaml -version



# Install Lwt's development dependencies.
make dev-deps

if [ "$LIBEV" != no ]
then
    opam install -y conf-libev
fi



# Build and run the tests.
if [ "$LIBEV" != no ]
then
    LIBEV_FLAG=true
else
    LIBEV_FLAG=false
fi

dune exec src/unix/config/configure.exe -- -use-libev $LIBEV_FLAG
make build
make test
make coverage



# Run the packaging tests.
make clean
make install-for-packaging-test
make packaging-test
make uninstall-after-packaging-test



# Run the ppx_let integratio test.
if [ "$COMPILER" != "4.02.3" ]
then
    make ppx_let-test-deps
    make ppx_let-test
fi

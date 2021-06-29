set -e
set -x

if [ "$APPVEYOR_SCHEDULED_BUILD" == True ]
then
    rm -rf ~/.opam
    rm -rf ./_cache
fi

date

opam init default https://github.com/fdopen/opam-repository-mingw.git#opam2 -c ocaml-variants.4.09.0+mingw64c --disable-sandboxing --yes --auto-setup

date

make dev-deps
opam clean

eval `opam config env`

opam --version
ocaml -version

# You can twiddle this comment to invalidate the cache.

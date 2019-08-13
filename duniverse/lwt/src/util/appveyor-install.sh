set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$SYSTEM-$COMPILER-$LIBEV.tar

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    # Pin Lwt and install its dependencies.
    make dev-deps
    if [ "$LIBEV" = yes ]
    then
        opam install -y conf-libev
    fi

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi

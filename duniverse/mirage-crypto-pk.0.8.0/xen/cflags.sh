#!/bin/sh

pkg_exists() {
    pkg-config --exists "mirage-xen-posix"
}
if ! pkg_exists; then
    export PKG_CONFIG_PATH="$(opam config var lib)/pkgconfig"
fi
pkg_exists || exit 1

flags="$(pkg-config --static mirage-xen-posix --cflags)"
echo "($flags)"

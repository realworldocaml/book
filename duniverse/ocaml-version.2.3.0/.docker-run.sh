#!/bin/bash

DISTRO=${DISTRO:-alpine}
OCAML_VERSIONS=${OCAML_VERSIONS:4.03 4.04 4.05 4.06 4.07}
docker run -it -e DISTRO=${DISTRO} -e OCAML_VERSIONS="${OCAML_VERSIONS}" -v `pwd`:/home/opam/src ocaml/opam2:${DISTRO} /home/opam/src/.docker.sh

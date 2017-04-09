#!/bin/sh -ex

docker build -t rwo-local -f Dockerfile.local .
rm -rf _build
docker run -v `pwd`:/home/opam/src -it rwo-local

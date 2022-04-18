#
# Build the atd project from source. Uses an image with all the dependencies
# already installed.
#
FROM atd-deps

COPY --chown=opam:opam . atd
WORKDIR /home/opam/atd

RUN opam exec -- make
RUN opam exec -- make test

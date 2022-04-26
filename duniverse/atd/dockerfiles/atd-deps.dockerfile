#
# Build an image with all the dependencies needed to build and test the atd
# project offline.
#
FROM ocaml/opam:ubuntu-20.04-ocaml-4.13

COPY .circleci/setup-* *.opam tmp/
RUN cd tmp && ./setup-system
RUN cd tmp && ./setup-opam

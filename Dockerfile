FROM ocaml/opam:ubuntu-16.04_ocaml-4.06.0
RUN sudo apt-get update && sudo apt-get -y install python-pygments
RUN opam repo set-url default https://opam.ocaml.org/
ENV OPAMYES=1
ENV OPAMJOBS=3
WORKDIR /home/opam/src

# install dependencies
COPY rwo.opam /home/opam/src/rwo.opam
RUN opam pin add -n rwo .
RUN opam depext -y rwo
RUN opam install --deps-only rwo

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam update rwo
RUN opam config exec -- make

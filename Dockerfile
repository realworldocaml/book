FROM ocaml/opam:ubuntu-17.04_ocaml-4.04.2
RUN sudo apt-get -y install python-pygments
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam depext -i cohttp-lwt-unix
# get this repository
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n rwo-book /home/opam/src
RUN opam pin add -n rwo-examples /home/opam/src/examples
RUN opam pin add -n rwo /home/opam/src/bin
RUN opam depext rwo-book rwo-examples rwo
RUN opam install rwo
RUN opam install rwo-book rwo-examples --deps-only
WORKDIR /home/opam/src/examples
RUN opam config exec -- make
WORKDIR /home/opam/src
RUN opam config exec -- make PYGMENTIZE=1 -j2

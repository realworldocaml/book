FROM ocaml/opam:ubuntu
RUN sudo apt-get update && sudo apt-get -y install python3-pygments tzdata pandoc texlive-full

# update opam
RUN opam switch 4.13
RUN git -C /home/opam/opam-repository pull origin master && opam update -uy

# install non-OCaml dependencies
WORKDIR /home/opam/src
COPY Makefile /home/opam/src/.
COPY rwo.opam /home/opam/src/.
RUN opam pin add -n rwo /home/opam/src && opam depext -y rwo
RUN opam install dune=3.1.1

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam exec -- dune build @site @pdf

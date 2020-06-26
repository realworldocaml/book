FROM ocaml/opam2:ubuntu-18.04
RUN sudo apt-get update && sudo apt-get -y install python-pygments tzdata
ENV OPAMYES=1
WORKDIR /home/opam/src

# update opam
RUN opam switch 4.10
RUN git -C /home/opam/opam-repository pull origin master && opam update -uy

# install non-OCaml dependencies
COPY Makefile /home/opam/src/.
COPY rwo.opam /home/opam/src/.
RUN opam pin add -n rwo . && opam depext -y rwo
RUN opam install dune=2.6.0

#install pandoc
WORKDIR /tmp
RUN curl -OL https://github.com/jgm/pandoc/releases/download/2.9/pandoc-2.9-1-amd64.deb && sudo dpkg -i pandoc-2.9-1-amd64.deb
WORKDIR /home/opam/src

#install pdflatex
WORKDIR /tmp
RUN sudo apt-get update && sudo apt-get -y install texlive-full
WORKDIR /home/opam/src

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam exec -- make
RUN opam exec -- make test

FROM ocaml/opam2:ubuntu-18.04
RUN sudo apt-get update && sudo apt-get -y install python-pygments tzdata
ENV OPAMYES=1
WORKDIR /home/opam/src

# update opam
RUN opam switch 4.07
RUN git -C /home/opam/opam-repository pull origin master && opam update -uy

# install non-OCaml dependencies
COPY Makefile /home/opam/src/.
RUN make depext
RUN opam install dune=1.11.4

#install pandoc
RUN curl -OL https://github.com/jgm/pandoc/releases/download/2.1.3/pandoc-2.1.3-1-amd64.deb && sudo dpkg -i pandoc-2.1.3-1-amd64.deb && rm -f pandoc-2.1.3-1-amd64.deb

#install pdflatex
RUN sudo apt-get update && sudo apt-get -y install texlive-full

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam exec -- make
RUN opam exec -- make test

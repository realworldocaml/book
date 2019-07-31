FROM ocaml/opam2:ubuntu-18.04
RUN sudo apt-get update && sudo apt-get -y install python-pygments tzdata
ENV OPAMYES=1
WORKDIR /home/opam/src

# update opam
RUN opam switch 4.06
RUN git -C /home/opam/opam-repository pull origin master && opam update -uy

# pre-install dependencies
RUN opam depext -y core async ppx_sexp_conv dune \
    toplevel_expect_test patdiff lambdasoup sexp_pretty fmt re mdx
    # Required for code blocks
    # core_bench mtime yojson astring cryptokit ocp-index atd atdgen ctypes \
    # ctypes-foreign textwrap uri
    # cohttp-async
RUN opam install dune=1.11.0

#install pandoc
WORKDIR /tmp
RUN curl -OL https://github.com/jgm/pandoc/releases/download/2.1.3/pandoc-2.1.3-1-amd64.deb && sudo dpkg -i pandoc-2.1.3-1-amd64.deb
WORKDIR /home/opam/src

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam exec -- make

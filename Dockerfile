FROM ocaml/opam:ubuntu-16.04_ocaml-4.06.0
RUN sudo apt-get update && sudo apt-get -y install python-pygments tzdata
RUN opam repo set-url default https://opam.ocaml.org/
ENV OPAMYES=1
ENV OPAMJOBS=3
WORKDIR /home/opam/src

# pre-install dependencies
RUN opam depext -iy core async ppx_sexp_conv ppx_deriving cohttp jbuilder \
    toplevel_expect_test patdiff cohttp-async lambdasoup ocamlnet sexp_pretty \
    core_bench mtime yojson astring cryptokit ocp-index atd atdgen ctypes \
    ctypes-foreign textwrap uri

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam config exec -- make

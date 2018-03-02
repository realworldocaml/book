FROM ocaml/opam:ubuntu-16.04_ocaml-4.06.0
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam repo set-url default https://opam.ocaml.org/
ENV OPAMYES=1
ENV OPAMJOBS=3
RUN opam depext -ui cohttp-lwt-unix async core_extended textwrap ctypes-foreign toplevel_expect_test sexp_pretty lambdasoup
RUN sudo apt-get -y install python-pygments
WORKDIR /home/opam/src

# install dependencies
COPY rwo.opam /home/opam/src/rwo.opam
RUN opam pin add -n rwo .
RUN opam depext -uy rwo
RUN opam install --deps-only rwo

# compile the project
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam update rwo
RUN opam config exec -- make

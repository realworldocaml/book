FROM ocaml/opam:ubuntu-17.04_ocaml-4.05.0
RUN sudo apt-get -y install python-pygments 
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam repo set-url default https://opam.ocaml.org/
ENV OPAMYES=1
ENV OPAMJOBS=4
RUN opam depext -j2 -i cohttp-lwt-unix async core_extended textwrap ctypes-foreign ocamlnet toplevel_expect_test
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n rwo .
RUN opam depext -uy rwo
RUN opam install --deps-only rwo -j2
RUN opam config exec -- make PYGMENTIZE=1 -j2
RUN rm -rf /home/opam/.opam

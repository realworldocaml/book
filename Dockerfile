FROM ocaml/opam:ubuntu-17.04_ocaml-4.05.0
RUN sudo apt-get -y install python-pygments 
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam repo set-url default https://opam.ocaml.org/
ENV OPAMYES=1
ENV OPAMJOBS=4
RUN opam depext -i cohttp-lwt-unix async core_extended
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n rwo-book .
RUN opam pin add -n rwo-examples .
RUN opam pin add -n rwo .
RUN opam depext -uy rwo rwo-examples rwo-book
RUN opam pin add ocaml-topexpect --dev
RUN opam install --deps-only rwo rwo-examples rwo-book -j4
RUN opam config exec -- jbuilder build
RUN opam config exec -- make PYGMENTIZE=1 -j2

FROM ocaml/opam:ubuntu-17.04_ocaml-4.04.2
RUN sudo apt-get -y install python-pygments
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam depext -i cohttp-lwt-unix
# until rwo.2.0.0 is released
RUN opam pin add -n rwo https://github.com/realworldocaml/scripts.git#v2
# get the evaluated examples, from the v2-sexp branch
RUN git clone -b v2-sexp git://github.com/realworldocaml/examples /home/opam/examples
# get this repository
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n rwo-book /home/opam/src
RUN opam depext rwo-book
RUN opam install rwo-book --deps-only
RUN opam config exec -- make
EXPOSE 8080
ENTRYPOINT ["opam","config","exec","--","cohttp-server-lwt","-v","/home/opam/src/site"]

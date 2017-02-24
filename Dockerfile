FROM ocaml/opam:alpine_ocaml-4.03.0
RUN opam remote add js git://github.com/janestreet/opam-repository && opam update
RUN opam depext -ui core async ocamlnet cohttp mtime
RUN opam depext -i toplevel_expect_test
RUN sudo apk add tzdata
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n Real-World-OCaml /home/opam/src
RUN opam config exec -- make
EXPOSE 8080
ENTRYPOINT ["opam","config","exec","--","cohttp-server-async","-v","true","/home/opam/src/_build/site"]

FROM ocaml/opam:alpine_ocaml-4.03.0
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam remote add js git://github.com/janestreet/opam-repository && opam update
RUN opam pin add -n uri --dev
RUN opam pin add -n ipaddr --dev
RUN opam depext -ui core async ocamlnet cohttp mtime toplevel_expect_test sexplib ppx_tools
RUN sudo apk add tzdata vim
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n Real-World-OCaml /home/opam/src
RUN opam config exec -- make
EXPOSE 8080
ENTRYPOINT ["opam","config","exec","--","cohttp-server-async","-v","/home/opam/src/_build/site"]

FROM ocaml/opam:alpine_ocaml-4.04.1
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam depext -i ocaml-migrate-parsetree ppx_sexp_conv
RUN opam pin add -n uri --dev
RUN opam pin add -n ipaddr --dev
RUN opam pin add -n -y ocaml-topexpect https://github.com/let-def/topexpect.git
RUN opam depext -ui async ocamlnet cohttp mtime ocaml-topexpect sexplib toplevel_expect_test ocamlscript
RUN sudo apk add tzdata vim
RUN sudo apk add 'py-pygments=2.2.0-r0' --update-cache --repository http://nl.alpinelinux.org/alpine/edge/main
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n Real-World-OCaml /home/opam/src
RUN opam config exec -- jbuilder build
EXPOSE 8080
ENTRYPOINT ["opam","config","exec","--","cohttp-server-async","-v","/home/opam/src/_build/default/site"]

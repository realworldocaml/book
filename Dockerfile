FROM ocaml/opam:ubuntu-17.04_ocaml-4.04.2
RUN sudo apt-get -y install python-pygments
RUN git -C /home/opam/opam-repository pull origin master && opam update
RUN opam pin add -n -y ocaml-topexpect https://github.com/let-def/topexpect.git
RUN opam depext -ui cohttp-async async ocamlnet cohttp mtime ocaml-topexpect sexplib toplevel_expect_test patdiff
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN opam pin add -n rwo /home/opam/src
RUN opam depext -iyj4 rwo
RUN opam config exec -- jbuilder build @site/book
EXPOSE 8080
ENTRYPOINT ["opam","config","exec","--","cohttp-server-async","-v","/home/opam/src/_build/default/site"]

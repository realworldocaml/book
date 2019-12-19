ppxfind - ocamlfind ppx tool
============================

Ppxfind is a small command line tool that allow to apply ppx rewriters
installed on the system on a file. It supports both new style ppx
rewriters (driverised) and old styles ones.

At the moment new styles ppx rewriters are executed in byte-code mode
as Ppxfind relies on dynamic loading and the packaging of a lot of ppx
rewriters is incomplete, i.e. the cmxs files are missing.

Using old styles ppx rewriters with jbuilder
--------------------------------------------

Ppxfind allows to use old style ppx rewriters with jbuilder. This is
not the recommended way of using ppx rewriters with jbuilder and in
particular it is slower and breaks composability. However, if you need
to use a ppx rewriter that is not compatible with the new style with
jbuilder, you can use Ppxfind as a workaround. Simply write this in
your `jbuild` file:

```scheme
  (preprocess (action (run ppxfind -legacy ppx1,ppx2,... --as-pp ${<})))
```

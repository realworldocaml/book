## rev-deps.sh

You can use this script to fetch ppxlib's rev-deps and clone them locally to test them
against the latest changes in ppxlib and eventually send patch to them.
Eventually this will make it into a duniverse feature but in the meantime it can prove useful
to reproduce the steps we went through to send patches ahead of the 0.14.0 release.

This script is used to assemble a dune-workspace containing all you need to build ppxlib and a
co-installable subset of its reverse dependencies together. It's split into three steps.

### Getting the rev deps locally

The first step is to actually compute the list of rev-deps we want and clone them. For this you can
run the following, from the root of the repo:
- `./dev/rev-deps.sh pull` if you want the non-JaneStreet rev deps.
- `./dev/rev-deps.sh pull janestreet` if you want only the JaneStreet packages.

JS and non-JS ppx-es are handled separately because they tend to be non-coinstallable. Furthermore
we want to send patches to the JS ppx-es on top of the latest release branch instead of the master
branch for other ppx-es. When cloned, the JS rev deps will be checked out to the latest release
branch.

The script will clone them in a `dunireverse` folder at the repo's root.

### Installing their dependencies

To install the dependencies using opam-monorepo, you can run
`./dev/rev-deps.sh install-deps`

That gets the sources locally by pulling them into `duniverse/` in your dune-workspace.

To get the opam monorepo plugin, required to assemble the duniverse with all the dependencies,
simply install it through opam:
```
opam install opam-monorepo
```

### Building them

Since some of the cloned repos might contain more than just the ppx-es we're interested in,
running just `dune build` might not do it. You can use `./dev/rev-deps.sh build` which will build
exactly what you need, ie ppxlib and all the rev-deps packages you cloned.

No black magic here, it's just running `dune build -p ppxlib,...` where `...` is the list of
rev-deps packages. The `-p` is also helpful to avoid annoying warnings getting in the way.

### Notes

This is all very experimental and sometimes a bit of extra work is required. This section contains
note that can hopefully help you with this process.

A good thing to do is to deal with janestreet packages first because if some non-janestreet rev-deps
depend on a janestreet package you can then simply pin to your patch before running the
`install-deps` step.

When last assembling the non janestreet rev-deps duniverse I had to remove the following packages:
- `elpi` as it depends on `camlp5`
- `gen_js_api` which depends on `omp.1.x` directly
- `obus` as it depends on `lwt_ppx` which uses `omp.1.x`
- `ppx_import` as it depends on `omp.1.x` directly
- `ppx_show` depends on stdcompat which doesn't build with dune
- `ppx_string_interpolation` depends on `sedlex.ppx` which uses OMP and `ppx_tools_versioned`

When last assembling the janestreet rev-deps duniverse I had to remove the following packages:
- `memtrace_viewer` as the repo is weirdly maintained, there's no tag for the released versions and
  the master branch's opam file depends on packages not available in opam at the time:
  `async_rpc_websocket` and `ocaml-embed-file`
- `ppx_python` as it depends on `pyml` which doesn't build with dune

`opam-monorepo` will pull in `dune-configurator` and if you're using a recent version of dune this
will conflict with the one you have locally so you should probably run:
```
rm -rf duniverse/dune-configurator*
```

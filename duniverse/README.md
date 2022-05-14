# duniverse

This folder contains vendored source code of the dependencies of the project,
created by the [opam-monorepo](https://github.com/ocamllabs/opam-monorepo)
tool. You can find the packages and versions that are included in this folder
in the `.opam.locked` files.

To update the packages do not modify the files and directories by hand, instead
use `opam-monorepo` to keep the lockfiles and directory contents accurate and
in sync:

```sh
opam monorepo lock 
opam monorepo pull
```

If you happen to include the `duniverse/` folder in your Git repository make
sure to commit all files:

```sh
git add -A duniverse/
```

For more information check out the homepage and manual of `opam-monorepo`.

ATD Contribution Guidelines
==

This is a collection of guides to help contributors to the ATD
project.

Whenever possible, we prefer to automate things over having people
read documents and follow rules.

Releasing `atd`
--

The release process involves assigning a
[version ID](https://semver.org/), tagging a git commit with this
version ID, building an archive, and publishing the opam packages that
use this archive.
[dune-release](https://github.com/ocamllabs/dune-release) makes this
process easy and safe. Refer to its documentation for more information.

Note that:
* We run the release steps directly on the main branch. We could
  resort to creating a branch if pushing to the main branch was
  restricted or if there was significant material to review.
* The point of no return is `dune-release publish`. If there's a
  failure after that, the release ID should be incremented and all the
  steps should be followed again.

1. Run `make opam-files` to make sure the opam files are up-to-date.
2. Review and update the changelog `CHANGES.md`.
3. Create a section with the desired version e.g. `2.3.0
   (2022-03-10)`.
4. Commit the changes.
5. Install [dune-release](https://github.com/ocamllabs/dune-release)
   if not already installed:
   `opam install dune-release`
6. Run `dune-release tag`. It will pick up the version from the
   changelog and ask for confirmation.
7. Run `dune-release distrib` to create a tarball.
8. Run `dune-release publish` to upload the tarball to GitHub and
   create GitHub release including the changes extracted from the
   changelog.
9. Create opam packages with `dune-release opam pkg`.
10. Submit the opam packages to opam-repository using
   `dune-release opam submit`.
11. Fix the opam-repository pull request as needed. For example, this
    may require setting a new version constraint on the `atd` package
    in the opam files, if it wasn't possible to do so in
    `dune-project`.
12. Check whether opam-repository's CI test succeed and fix problems
    accordingly until the pull request is merged.

Shortcut for all the `dune-release` steps:
```
$ make opam-release
```

Contributing to a specific subproject
--

Each subproject has its own README:

* [atdgen](atdgen): targets OCaml, Bucklescript
* [atdj](atdj): targets Java
* [atdpy](atdpy): targets Python
* [atds](atds): targets Scala
* [atdts](atdts): targets TypeScript

Updating the documentation
--

### Documentation setup

The user documentation is published at https://atd.readthedocs.io/.
It's automatically published from the main branch of the GitHub repo
from the files found in `/doc`.

The [format of the documentation is Restructured
Text](https://thomas-cokelaer.info/tutorials/sphinx/rest_syntax.html#restructured-text-rest-and-sphinx-cheatsheet)
because it's more expressive that Markdown and it's not that hard to
pick up.

You can either edit the `.rst` files and hope that everything will
turn out fine or you can preview it by running Sphinx locally. The
latter is recommended for large edits. Try this:

Install sphinx and the theme we're using:
```
pip install -r doc/requirements.txt
```

Compile the documentation and run a local
[HTTP server on port 8888](http://0.0.0.0:8888):
```
make livedoc
```

### Writing good documentation

Don't assume that our existing documentation is already good (!)

Good documentation separates concerns. This not only makes it easy
to read but also easier to write.
Daniele Procida has a wonderful presentation about the four kinds of
documentation and why they're best kept separate:

* [website](https://documentation.divio.com/)
* [30-min presentation](https://www.youtube.com/watch?v=t4vKPhjcMZg)

![the four kinds of documentation](https://user-images.githubusercontent.com/343265/153692997-07752757-2227-4ba7-b53f-e5e85a71e71a.png)

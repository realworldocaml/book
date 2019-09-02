atdgen-doc
==========

Documentation for atdgen, published at http://mjambon.github.io/atdgen-doc/

How to build and review the documentation
-----------------------------------------

Fork and clone the repository as usual:

```
$ git clone git@github.com:mjambon/atdgen-doc.git
$ cd atdgen-doc
```

Build the documentation (html, man pages) using `make`. Review the
HTML pages by opening the files from `html/` in a web browser. Review
the man pages using e.g. `MANPATH=. man atdgen`.

How to publish the documentation
--------------------------------

Publishing is done by putting the generated files into the
branch `gh-pages`. It is recommended to work on a separate clone for
that. We recommend:

```
$ git clone git@github.com:mjambon/atdgen-doc.git atdgen-doc.gh-pages
$ cd atdgen-doc.gh-pages
$ git checkout gh-pages
```

After changes have been made to the sources on the `master` branch,
merge them into the `gh-pages` branch, then build the web pages using
`make`, and finally commit and push the changes, which effectively
publishes the pages:

```
$ git pull origin master
$ make
$ git commit -a
$ git push origin gh-pages
```

The pages will show up under https://mjambon.github.io/atdgen-doc/

Note that as a side effect of how we manage the documentation,
the source files end up also being published, e.g. one can fetch
https://mjambon.github.io/atdgen-doc/src/Makefile

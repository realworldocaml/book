Easy-format: indentation made easy
==================================


Documentation
-------------

The documentation is at https://mjambon.github.io/mjambon2016/easy-format.html


Installation
------------

Installation requires ocamlfind.

```
$ make
$ make install
```

Uninstallation
--------------

```
$ make uninstall
```

Examples
--------

Some examples can be run with `make test` before installing.

More examples that require Easy-format to be installed and possibly
dependencies on other packages can be found in the `examples/` subdirectory.

TODO
----

* Port documentation from old website to a better place, possibly https://readthedocs.org/.
* Switch from pure `make` to `jbuilder` for both build/install and development.
* We're looking for an official maintainer for the library and its documentation.

`cppo-json` is a preprocessor that replaces embedded type definition 
directives with OCaml type definitions and JSON
serialization/deserialization code.

`atdgen-cppo` is the script that reads type definitions from stdin and
generates OCaml code. It takes options allowing users to pick what
kind of code needs to be generated (type definitions, JSON
serialization, Biniou serialization, validators).

Example
-------

Sample input:

```
$ cat example.ml
#ext json
type mytype = string list
#endext
let data = [ "Hello"; "world" ]
let () = print_endline (J.string_of_mytype data)
```

How to view the OCaml code produced by cppo-json:

```
$ cppo-json < example.ml | less
```

How to compile an OCaml program:

```
$ ocamlfind opt -o example \
    -pp cppo-json \
    -package atdgen -linkpkg \
    example.ml
```

cppo-json ships with atdgen-cppo and is shorthand for the following command:

```
cppo -x "json:atdgen-cppo t j v"
```

where `t` stands for "type definitions", `j` stands for "JSON", and
`v` stands for "validators".

See also:
```
$ cppo-json --help
$ atdgen-cppo --help
$ cppo --help
```


Documentation
-------------

Documentation is provided by the `--help` option of each command.

Direct dependencies
-------------------

* [atdgen](https://github.com/MyLifeLabs/atdgen)
* [cppo](https://github.com/mjambon/cppo)

Installation
------------

It's just two shell scripts. You can copy them by hand to the
directory of your choice or run:

```
$ make install   # installs into $HOME/bin
```

or

```
$ BINDIR=/path/to/bin make install
```

textwrap
========

An almost complete port of Python's [textwrap] [1] library to OCaml.

Installation
------------

Install using the [Dune](https://dune.build) build tool:

    $ dune build
    $ dune install

or for immediate use:

    $ dune utop

Or using [opam 2.0](https://opam.ocaml.org):

    $ opam install .

Example
-------

So, what's in the box?

    # let w = Wrapper.make 4 in
      print_endline (Wrapper.fill w "Hello world!");;
    Hell
    o wo
    rld!
    - : unit = ()

Here's a table of `Wrapper.make` arguments, controlling various aspects
of wrapping:

```
| argument             | default | description                                        |
|----------------------+---------+----------------------------------------------------|
| initial_indent       | ""      | string that will be prepended to the first line    |
|                      |         | of wrapped output.                                 |
| subsequent_indent    | ""      | string that will be prepended to all but the       |
|                      |         | first lines of wrapped output.                     |
| expand_tabs          | true    | expand tabs in input string to spaces before       |
|                      |         | further processing -- each tab will become         |
|                      |         | 8 spaces; if `false`, each tab is treaded as a     |
|                      |         | single character.                                  |
| replace_whitespace   | true    | replace all whitespace characters in the input     |
|                      |         | text by spaces *after* tab expansion.              |
| fix_sentence_endings | false   | ensure that sentence-ending punctuation is         |
|                      |         | allways followed by two spaces.                    |
| break_long_words     | true    | break words longer than 'width', if `false`, those |
|                      |         | words won't be broken and some lines might be      |
|                      |         | longer that 'width'.                               |
| drop_whitespace      | true    | drop leading and trailing whitespace from lines.   |
```

Missing features
----------------

The only feature OCaml version is missing from the original [textwrap] [1]
is [`break_on_hyphens`] [2], which requires a decent regex engine to
implement; unfortunately, this is not the case for [`Str`] [3].

[1]: http://docs.python.org/library/textwrap
[2]: http://hg.python.org/cpython/file/ca2a35140e6a/Lib/textwrap.py#l75
[3]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html

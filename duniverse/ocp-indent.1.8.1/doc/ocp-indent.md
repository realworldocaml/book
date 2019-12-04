# ocp-indent

A simple tool to indent OCaml programs

Authors: Louis Gesbert (OCamlPro), Thomas Gazagnaire (OCamlPro), Jun Furuse

License: LGPL 2.1 with linking exception

## Installation

### Using OPAM

The simplest way to install `ocp-indent` is using [OPAM](http://opam.ocamlpro.com):

```bash
opam install ocp-indent
```

### By hand

You can also compile and install `ocp-indent` from sources. You'll need `ocaml
(>= 3.12.1)` and `ocp-build (>= 1.99.6-beta)`:

```bash
./configure
make
make install
```

If you use opam and want it installed alongside ocaml, you may want to use
`./configure --prefix $(opam config var prefix)`.

## Usage

The above installation step copies elisp scripts to
`<prefix>/share/emacs/site-lisp/` and vim scripts to
`<prefix>/share/ocp-indent/vim/`. You then need to load them in the editor of
your choice to automatically use ocp-indent.

Installing OPAM package
[`user-setup`](https://opam.ocaml.org/packages/user-setup/user-setup.0.3/) will
trigger automatic configuration for popular editors (emacs and vim currently,
but more are in the works). If you prefer to handle your configuration manually,
read on.

### Emacs

Run the following command to setup tuareg-mode or caml-mode to use `ocp-indent`
for indentation:

```bash
echo '(load-file "'"$(opam config var share)"'/emacs/site-lisp/ocp-indent.el")' >>~/.emacs
```

The `tab` key should now reindent the current line using ocp-indent.

### Vim

Use the following command to tell Vim to use `ocp-indent` to indent OCaml code:

```bash
echo 'set rtp^="'"$(opam config var ocp-indent:share)"'/vim"' >>~/.vimrc
```

Automatic indentation as you type should take place, depending on your
configuration. Use `==` to reindent the current line, and `=G` to reindent until
the end of buffer.

### Other editors

As `ocp-indent` is a command-line tool, you can easily integrate it with other editors.

```bash
ocp-indent <src-file> > <dst-file>
```

You can also tell it to indent only a subsets of lines, and to output only the indentation level:

```bash
ocp-indent <src-file> --lines <l1>-<l2> --numeric
```

## Configuration options

By default, `ocp-indent` comes with sensible default parameters. However,
you can customize some of the indentation options using command-line
arguments. For more details, see:

```bash
ocp-indent --help
```

### Configuration file
The same parameters can be defined in a configuration file, allowing for user
defaults and per-project parameters. The latter is particularly convenient to
transparently ensure consistency in projects with many contributors, without
requiring them to change their settings in any way (except that, obviously, they
need to use ocp-indent !).

If a `.ocp-indent` file is found in the current directory or its ancestors, it
overrides definitions from `$XDG_CONFIG_HOME/ocp/ocp-indent.conf`,
`~/.ocp/ocp-indent.conf` and the built-in default. The command-line can of
course still be used to override parameters defined in the files.

Have a look at ocp-indent's own [`.ocp-indent`](.ocp-indent) file for an
example.

### In-file configuration
There is no built-in support for in-file configuration directives. Yet, some
editors already provide that features, and with emacs, starting your file with a
line like:

```
(* -*- ocp-indent-config: in=2 -*- *)
```

will enable you to have the indentation after `in` setup to 2 locally on this
file.


## How does it compare to tuareg ?

We've run some benchmarks on real code-bases and the result is quite
conclusive. Keep in mind than most of existing source files are
either indented manually or following tuareg standards. You can
see the results [here](http://htmlpreview.github.com/?https://github.com/AltGr/ocp-indent-tests/blob/master/status.html).

Moreover, as `ocp-indent` has a deep understanding of the OCaml syntax
it shines on specific cases. See for instance the collection of
unit-tests
[here](https://github.com/OCamlPro/ocp-indent/tree/master/tests/passing). The
currently failing tests can be seen
[here](http://htmlpreview.github.com/?https://github.com/OCamlPro/ocp-indent/blob/master/tests/failing.html).


## Testing

It's hard to deliver a great indenter without tests. We've built
`ocp-indent` based on a growing collection of unit-tests. If you find an
indentation bug, feel free to send us a code snippet that we will
incorporate into our test suite.

The tests are organized as follows:

* `tests/passing` contains tests that are properly indented and should be left
  unchanged by ocp-indent.
* `tests/failing` contains tests for which ocp-indent currently returns the
  results in `tests/failing-output`, hence `meld tests/failing{,-output}` should
  give an overview of currently known bugs (also available online
  [here](http://htmlpreview.github.com/?https://github.com/OCamlPro/ocp-indent/blob/master/tests/failing.html)).
* `tests/test.sh` checks the current state against the reference state (checked
  into git).
* `tests/test.sh --[git-]update` updates the current reference state.
* See `tests/test.sh --help` for more

Please make sure tu run `make && tests/test.sh --git-update` before any commit,
so that the repo always reflects the state of the program.

# Installation

The easiest way to use OCaml is via the binary packages available in
many operating systems.  For day-to-day code development however, it's
much easier to use a source-code manager that lets you modify
individual libraries and automatically recompile all the dependencies.

An important difference between OCaml and scripting languages such as
Python or Ruby is that the static type safety that means that you
can't just mix-and-match compiled libraries.  Interfaces are checked
when libraries are compiled, so when an interface is changed, all the
dependent libraries must also be recompiled.  Source-based package
managers automate this process for you and make development life much
easier.

To work through Real World OCaml, you'll need three major components
installed:

* The OCaml compiler itself.
* The OPAM source package manager, through which we'll install several
  extra libraries.
* The `utop` interactive toplevel, a modern interactive toplevel with
  command history and tab completion.

Let's get started with how to install OCaml on various operating
systems, and we'll get OPAM and `utop` running after that.

## Getting OCaml 

The OCaml compiler is available as a binary distribution on many
operating systems.  This is the simplest and preferred installation
route, but we'll also describe how to do a manual installation as a
last resort.

### MacOS X

The [Homebrew](http://github.com/mxcl/homebrew) package manager has an
OCaml installer, which is usually updated pretty quickly to the latest
stable release.

```
$ brew install ocaml
$ brew install pcre
```

The Perl-compatible Regular Expression library (PCRE) is used by the
Core suite.  It's not strictly needed to use OCaml, but is a commonly
used library that we're installing now to save time later.

Another popular package manager on MacOS X is
[MacPorts](http://macports.org), which also has an OCaml port:

```
$ port install ocaml
$ port install ocaml-pcre
```

### Debian Linux

On Debian Linux, you should install OCaml via binary packages.  You'll
need at least OCaml version 3.12.1 to bootstrap OPAM, which means
using Debian Wheezy or greater.  Don't worry about getting the
absolute latest version of the compiler, as you just need one new
enough to compile the OPAM package manager, after which you use OPAM
to manage your compiler installation.

```
$ sudo apt-get install ocaml ocaml-native-compilers camlp4-extra
$ sudo apt-get install git libpcre3-dev curl build-essential m4 
```

Notice that we've installed a few more packages than just the OCaml
compiler here.  The second command line installs enough system
packages to let you build your own OCaml packages.  You may find that
some OCaml libraries require more system libraries (for example,
`libssl-dev`), but we'll highlight these in the book when we introduce
the library.

TODO: Fedora / RHEL

### Arch Linux

Arch Linux provides OCaml 4.0.1 (or later) in the standard repositories, so the
easiest method of installation is using `pacman`:

```
$ pacman -Sy ocaml
```

### Windows

Windows is not currently supported by the examples in Real World
OCaml, although it is being worked on.  Until that's ready, we
recommend using a virtual machine running Debian Linux on your local
machine.

### Building from source

_NOTE_: we really should be telling people to install 4.01, but
it's not released yet.

To install OCaml from source code, first make sure that you have a C
compilation environment (usually either `gcc` or `llvm` installed)

```
$ curl -OL http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-4.00.1.tar.gz
$ tar -zxvf ocaml-4.00.1.tar.gz
$ cd ocaml-4.00.1
$ ./configure
$ make world world.opt
$ sudo make install
```

The final step requires administrator privilege to install in your
system directory.  You can also install it in your home directory by
passing the `prefix` option to the configuration script:

```
$ ./configure -prefix $HOME/my-ocaml
```

Once the installation is completed into this custom location, you will
need to add `$HOME/my-ocaml/bin` to your `PATH`, normally by editing
the `~/.bash_profile` file.  You shouldn't really to do this unless
you have special reasons, so try to install binary packages before
trying a source installation.

## Getting OPAM

OPAM manages multiple simultaneous OCaml compiler and library
installations, tracks library versions across upgrades, and recompiles
dependencies automatically if they get out of date.  It's used
throughout Real World OCaml as the mechanism to retrieve and use
third-party libraries.

Before installing OPAM, make sure that you have the OCaml compiler
installed as described above.  Once installed, the entire OPAM
database is held in your home directory (normally `$HOME/.opam`).  If
something goes wrong, just delete this `.opam` directory and start
over from a clean slate.  If youre using a version of OPAM you've
installed previously, please ensure you have at least version
0.9.3 or greater.

<important>
<title>OCamlfind and OPAM</title>

OPAM maintains multiple compiler and library installations, but this
can clash with a global installation of the `ocamlfind` tool.
Uninstall any existing copies of `ocamlfind` before installing OPAM.
_Reviewers_: this has since been fixed in OCaml-4.01.0.

</important>

### MacOS X

Source installation of OPAM will take a minute or so on a modern
machine.  There is a Homebrew package for the latest OPAM:

```
$ brew update
$ brew install opam
```

And on MacPorts, install it like this:

```
$ port install opam
```

### Debian Linux

There are experimental binary packages available for Debian
Wheezy/amd64. Just add the following line to your
`/etc/apt/sources.list`:

```
deb http://www.recoil.org/~avsm/ wheezy main
```

When this is done, update your packages and install OPAM.  You can
ignore the warning about unsigned packages, which will disappear when
OPAM is upstreamed into Debian mainline.

```
# apt-get update
# apt-get install opam
```

<note>
<title>Note to reviewers</title>

The OPAM instructions will be simplified when integrated upstream into
Debian and Fedora, which is ongoing.  Until then, we're leaving
source-code installation instructions here. Please leave a comment
with any amended instructions you encounter

</note>

If the binary packages aren't suitable, you need to install the latest
OPAM release from source.  The distribution only requires the OCaml
compiler to be installed, so this should be pretty
straightforward. Download the latest version, which is always marked
with a `stable` tag on the project
[homepage](https://github.com/OCamlPro/opam/tags).

```
$ curl -OL https://github.com/OCamlPro/opam/archive/latest.tar.gz
$ tar -zxvf latest.tar.gz
$ cd opam-latest
$ ./configure && make
$ sudo make install
```

### Fedora/RHEL

 TODO

### Arch Linux

OPAM is available in the Arch User Repository (AUR) in two packages.
You'll need both `ocaml` and the `base-devel` packages installed first:

* `opam` contains the most recent stable release, and is the recommended package.
* `opam-git` builds the package from the latest upstream source, and should only be used if you are looking for a specific bleeding-edge feature.

Run these commands to install the stable OPAM package:

```
$ sudo pacman -Sy base-devel 
$ wget https://aur.archlinux.org/packages/op/opam/opam.tar.gz 
$ tar -xvf opam.tar.gz && cd opam
$ makepkg 
$ sudo pacman -U opam-_version_.pkg.tar.gz
```

## Setting up OPAM

The entire OPAM package database is held in the `.opam` directory in
your home directory, including compiler installations. On Linux and
MacOS X, this will be the `~/.opam` directory.  You shouldn't switch
to an admin user to install packages as nothing will be installed
outside of this directory.  If you run into problems, just delete the
whole `~/.opam` directory and follow the installations instructions
from the `opam init` stage again.

Begin by initialising the OPAM package database.

```
$ opam init
$ opam list
```

You only need to run this command once, and it will create the
`~/.opam` directory and sync with the latest package list from the
online OPAM database.  `opam list` will list these, but don't install
any just yet.

The most important package we need to install is Core, which is the
replacement standard library that all of the examples in this book
use.  Before doing this, let's make sure you have exactly the right
compiler version you need.  We've made some minor modifications to the
way the OCaml compiler displays type signatures, and the next command
will install a patched `4.00.1` compiler with this functionality
enabled.

```
$ opam switch 4.00.1+short-types
```

This step will take about 5-10 minutes on a modern machine, and will
download and install (within the `~/.opam` directory) a custom OCaml
compiler.  OPAM supports multiple such installations, and you'll find
this very useful if you ever decide to hack on the internals of the
compiler itself, or you want to experiment with the latest release
without sacrificing your current installation.  You only need to
install this compiler once, and future updates will be much faster as
they only recompile libraries within the compiler installation.

The new compiler will be installed into `~/.opam/4.00.1+short-types`
and any libraries you install for it will be tracked separately from
your system installation.  You can have any number of compilers
installed simultaneously, but only one can be active at any time.
Browse through the available compilers by running `opam switch list`.

When the compilation finishes, you'll see some instructions about
environment variables.  OPAM never installs files into your system
directories (which would require administrator privileges).  Instead,
it puts them into your home directory by default, and can output a set
of shell commands which configures your shell with the right `PATH`
variables so that packages will just work.  This requires just one
command:

```
$ eval `opam config env`
```

This evaluates the results of running `opam config env` in your
current shell, and sets the variables so that subsequent commands will
use them.  This only works with your current shell, and it can be
automated for all future shells by adding the line to your login
scripts.  On MacOS X or Debian, this is usually the `~/.bash_profile`
file if you're using the default shell.  If you've switched to another
shell, it might be `~/.zshrc` instead.  OPAM isn't unusual in this
approach; the SSH `ssh-agent` also works similarly, so if you're
having any problems just hunt around in your configuration scripts to
see how that's being invoked.

Finally, we're ready to install the Core libraries.  Run this:

```
$ opam install core core_extended async
```

This will take about five minutes to install, and install a series of packages.
OPAM figures out the dependencies you need automatically, but the three
packages that really matter are:

* `core` is the main, well-supported Core distribution from Jane Street.
* `core_extended` contains a number of experimental, but useful,
  extension libraries that are under review for inclusion in Core.  We
  use some of these in places, but much less than Core itself.
* `async` is the network programming library that we use in Part II to
  communicate with other hosts.  You can skip this for the initial
  installation until you get to Part II, if you prefer.

### Editing Environment

There's one last tool you need before getting started on the examples.
The default `ocaml` command gives us an interactive command-line to
experiment with code without compiling it.  However, it's quite a
spartan experience and so we use a more modern alternative.

```
$ opam install utop
```

The `utop` package us an interactive command-line interface to OCaml
that has tab-completion, persistent history and integration with Emacs
so that you can run it within your editing environment.

Remember from earlier that OPAM never installs files directly into
your system directories, and this applies to `utop` too.  You'll find
the binary in `~/.opam/4.00.1+short-types/bin`.  However, just typing
in `utop` from your shell should just work, due to the `opam config
env` step which configures your shell.  Don't forget to automate this
as described earlier, as it makes life much easier when developing
OCaml code!

### Command Line

TODO: explain OCamlfind here.

The `utop` tool provides a convenient interactive toplevel, with full
command history, command macros and module name completion.  The
`~/.ocamlinit` file in your home directory initialises `utop` with
common libraries and syntax extensions, so you don't need to type them
in every time.  A good default you should create for the examples in
this book is:

```ocaml
#use "topfind"
#camlp4o
#thread
#require "core.top";;
#require "async";;
```

When you run `utop` with this initialization file, it should start up with
Core opened and ready to use.

_TODO_: the `.ocamlinit` handling in OPAM is being finalised and is
tracked in [issue 185](https://github.com/OCamlPro/opam/issues/185).

### Editors

TODO: Emacs users have tuareg and [Typerex](http://www.typerex.org/).

TODO: Vim users can use the built-in style, and
[ocaml-annot](http://github.com/avsm/ocaml-annot) may also be useful.

TODO: Eclipse plugins: which one is maintained?

## Developing with OPAM

TODO: Package listings are obtained by adding *remotes* that provide package
descriptions, installation instructions and URLs.

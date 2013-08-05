# Installation

The easiest way to use OCaml is via the binary packages available for
many operating systems.  For day-to-day code development however, it's
much easier to use a source-code manager that lets you modify
individual libraries and automatically recompile all the dependencies.

An important difference between OCaml and scripting languages such as Python or
Ruby is the static type safety that means that you can't just mix-and-match
compiled libraries.  Interfaces are checked when libraries are compiled, so
when an interface is changed, all the dependent libraries must also be
recompiled.  Source-based package managers automate this process for you and
make development life much easier.

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

### Mac OS X

The [Homebrew](http://github.com/mxcl/homebrew) package manager has an
OCaml installer, which is usually updated pretty quickly to the latest stable
release.  Make sure that you have the latest XCode (and Command Line Tools for
XCode) installed from the App Store before starting the OCaml installation.

```
$ brew install ocaml
$ brew install pcre
```

The Perl-compatible Regular Expression library (PCRE) is used by the
Core suite.  It's not strictly needed to use OCaml, but is a commonly
used library that we're installing now to save time later.

Another popular package manager on Mac OS X is
[MacPorts](http://macports.org), which also has an OCaml port.  As
with Homebrew, make sure you have XCode installed and have followed
the rest of the MacPorts installation instructions, and then type in:

```
$ sudo port install ocaml
$ sudo port install ocaml-pcre
```

### Debian Linux

On Debian Linux, you should install OCaml via binary packages.  You'll
need at least OCaml version 3.12.1 to bootstrap OPAM, which means
using Debian Wheezy or greater.  Don't worry about getting the
absolute latest version of the compiler, as you just need one new
enough to compile the OPAM package manager, after which you'll use OPAM
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

### Fedora and Red Hat

OCaml has been included in the basic distribution since Fedora 8.  To install
the latest compiler, just run:

```
# yum install ocaml
# yum install ocaml-camlp4-devel
# yum install pcre-devel
```

The PCRE package is used by Core and is just included here for convenience
later.

### Arch Linux

Arch Linux provides OCaml 4.00.1 (or later) in the standard repositories, so
the easiest method of installation is using `pacman`:

```
$ pacman -Sy ocaml
```

### Windows

Windows is not currently supported by the examples in Real World
OCaml, although it is being worked on.  Until that's ready, we
recommend using a virtual machine running Debian Linux on your local
machine.

### Building from source

To install OCaml from source code, first make sure that you have a C
compilation environment (usually either `gcc` or `llvm` installed).

```
$ curl -OL https://github.com/ocaml/ocaml/archive/trunk.tar.gz
$ tar -zxvf trunk.tar.gz
$ cd ocaml-trunk
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

<note>
<title>Note to reviewers</title>

We instruct you install the unreleased trunk version of OCaml
in these instructions, as we take advantage of some recent additions
to the language that simplify explanations in the book.  The 4.01
release will happen before the book is released, but you may run
into "bleeding edge" bugs with the trunk release.  Leave a comment
here if you do and we'll address them.

</note>

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
over from a clean slate.  If youre using a beta version of OPAM,
please upgrade it to at least version 1.0.0 or greater before
proceeding.

### Mac OS X

Source installation of OPAM will take a minute or so on a modern
machine.  There is a Homebrew package for the latest OPAM:

```
$ brew update
$ brew install opam
```

And on MacPorts, install it like this:

```
$ sudo port install opam
```

### Debian Linux

OPAM has recently been packaged for Debian and will soon be part of the unstable
distribution.  If you're on an earlier stable distribution such as `wheezy`, you
can either compile from source, or cherry-pick just the OPAM binary package from
`unstable` by:


```console
# apt-get update
# apt-get -t unstable install opam
```

<note>
<title>Note to reviewers</title>

The binary packages for OPAM are not yet available as of the 17th June 2013,
but the package is in the `NEW` queue.  It should be available by the the time
the book is released, and these instructions will be updated accordingly.

</note>

### Fedora and Red Hat

There is currently no RPM available for Fedora or Red Hat, so please
install OPAM via the source code instructions for the moment.

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

### Source Installation

If the binary packages aren't available for your system, you'll need to install
the latest OPAM release from source.  The distribution only requires the OCaml
compiler to be installed, so this should be straightforward.  Download
the latest version from the [homepage](https://github.com/OCamlPro/opam/tags).

```console
$ curl -OL https://github.com/OCamlPro/opam/archive/latest.tar.gz
$ tar -zxvf latest.tar.gz
$ cd opam-latest
$ ./configure && make
$ sudo make install
```

<note>
<title>Note to reviewers</title>

The OPAM instructions will be simplified when integrated upstream into
Debian and Fedora, which is ongoing.  Until then, we're leaving
source-code installation instructions here. Please leave a comment
with any amended instructions you encounter.

</note>


## Configuring OPAM

The entire OPAM package database is held in the `.opam` directory in
your home directory, including compiler installations. On Linux and
Mac OS X, this will be the `~/.opam` directory.  You shouldn't switch
to an admin user to install packages as nothing will be installed
outside of this directory.  If you run into problems, just delete the
whole `~/.opam` directory and follow the installations instructions
from the `opam init` stage again.

Let's begin by initialising the OPAM package database.  This will
require an active Internet connection, and ask you a few interactive
questions at the end.  It's safe to answer yes to these unless you
want to manually control the configuration steps yourself as an
advanced user.

```
$ opam init
<...>
=-=-=-= Configuring OPAM =-=-=-=
Do you want to update your configuration to use OPAM ? [Y/n] y
[1/4] Do you want to update your shell configuration file ? [default: ~/.profile] y
[2/4] Do you want to update your ~/.ocamlinit ? [Y/n] y
[3/4] Do you want to install the auto-complete scripts ? [Y/n] y
[4/4] Do you want to install the `opam-switch-eval` script ? [Y/n] y
User configuration:
  ~/.ocamlinit is already up-to-date.
  ~/.profile is already up-to-date.
Gloabal configuration:
  Updating <root>/opam-init/init.sh
    auto-completion : [true]
    opam-switch-eval: [true]
  Updating <root>/opam-init/init.zsh
    auto-completion : [true]
    opam-switch-eval: [true]
  Updating <root>/opam-init/init.csh
    auto-completion : [true]
    opam-switch-eval: [true]
```

You only need to run this command once, and it will create the
`~/.opam` directory and sync with the latest package list from the
online OPAM database.

When the `init` command finishes, you'll see some instructions about
environment variables.  OPAM never installs files into your system
directories (which would require administrator privileges).  Instead,
it puts them into your home directory by default, and can output a set
of shell commands which configures your shell with the right `PATH`
variables so that packages will just work.  This requires just one
command:

```
$ eval `opam config -env`
```

This evaluates the results of running `opam config env` in your
current shell, and sets the variables so that subsequent commands will
use them.  This only works with your current shell, and it can be
automated for all future shells by adding the line to your login
scripts.  On Mac OS X or Debian, this is usually the `~/.bash_profile`
file if you're using the default shell.  If you've switched to another
shell, it might be `~/.zshrc` instead.  OPAM isn't unusual in this
approach; the SSH `ssh-agent` also works similarly, so if you're
having any problems just hunt around in your configuration scripts to
see how that's being invoked.

If you answered `yes` to the auto-complete scripts question during
`opam init`, this should have all been set up for you. 
You can verify this worked by listing the available packages:

```
$ opam list
```

<note>
<title>Note to reviewers</title>

OPAM 1.0.0 places the login commands into your `~/.profile`
directory, which isn't always executed if your shell is `bash`.
This has been fixed in subsequent versions, but for now you'll need to manually
copy the contents of `~/.profile` over to `~/.bash_profile` via:

```
$ cat ~/.profile >> ~/.bash_profile
```

</note>

The most important package we need to install is Core, which is the
replacement standard library that all of the examples in this book
use.  Before doing this, let's make sure you have exactly the right
compiler version you need.  We've made some minor modifications to the
way the OCaml compiler displays type signatures, and the next command
will install a patched `4.01.0` compiler with this functionality
enabled.

```
$ opam switch 4.01.0dev+trunk
```

This step will take about 5-10 minutes on a modern machine, and will
download and install (within the `~/.opam` directory) a custom OCaml
compiler.  OPAM supports multiple such installations, and you'll find
this very useful if you ever decide to hack on the internals of the
compiler itself, or you want to experiment with the latest release
without sacrificing your current installation.  You only need to
install this compiler once, and future updates will be much faster as
they only recompile libraries within the compiler installation.

The new compiler will be installed into `~/.opam/4.01.0dev+trunk`
and any libraries you install for it will be tracked separately from
your system installation.  You can have any number of compilers
installed simultaneously, but only one can be active at any time.
Browse through the available compilers by running `opam switch list`.

Finally, we're ready to install the Core libraries.  Run this:

```
$ opam install core core_extended async
```

This will take about five or ten minutes to build, and will install a series of
packages.  OPAM figures out the dependencies you need automatically, but the
three packages that really matter are:

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

The `utop` package is an interactive command-line interface to OCaml
that has tab-completion, persistent history and integration with Emacs
so that you can run it within your editing environment.

Remember from earlier that OPAM never installs files directly into
your system directories, and this applies to `utop` too.  You'll find
the binary in `~/.opam/4.01.0dev+trunk/bin`.  However, just typing
in `utop` from your shell should just work, due to the `opam config
env` step that configures your shell.  Don't forget to automate this
as described earlier, as it makes life much easier when developing
OCaml code!

### Command Line

The `utop` tool provides a convenient interactive toplevel, with full
command history, command macros and module name completion.  When you
first run `utop`, you'll find yourself at an interactive prompt with
a bar at the bottom of the screen.  The bottom bar dynamically updates as you
write text, and contains the possible names of modules or variables
that are valid at that point in the phrase you are entering.  You can
press the `<tab>` key to complete the phrase with the first choice. 

The `~/.ocamlinit` file in your home directory initialises `utop` with
common libraries and syntax extensions so you don't need to type them
in every time.  Now that you have Core installed, you should update it
to load it every time you start `utop`, by adding this to it:

```ocaml
#use "topfind"
#camlp4o
#require "core.top"
#require "core.syntax"
```

You can also optionally add some more useful libraries that are used in
the book, such as `Core_extended` and `Async`.  You can also open the Core
module by default if that's all you ever use the top-level for.  Just append
these lines to the `.ocamlinit` file if that's what you prefer.

```
#require "core_extended"
#require "async"
open Core.Std
```

When you run `utop` with these initialization rules, it should start up with
Core opened and ready to use.

### Editors

<note>
<title>Note to reviewers</title>

The instructions for editor setup are still being compiled.  If you have a
relevant tip or HOWTO, then we'd *really* appreciate you leaving a note here
with a pointer or direct instructions.

</note>

#### Emacs

TODO: Emacs users have tuareg and [Typerex](http://www.typerex.org/).

To use `utop` directly in Emacs, add the following line to your `~/.emacs` file:

```scheme
(autoload 'utop "utop" "Toplevel for OCaml" t)
```

You also need to make the `utop.el` file available to your Emacs installation.
The OPAM version of `utop` installs it into the `~/.opam` hierarchy, for
example in `~/.opam/system/share/emacs/site-lisp/utop.el`. You may need to
replace `system` with your current compiler switch, such as `4.01.0dev+trunk`.

Once this successfully loads in Emacs, you can run utop by executing the
command `utop` in Emacs.  There are more details instructions at the 
[utop homepage](https://github.com/diml/utop#integration-with-emacs).

#### Vim

TODO: Vim users can use the built-in style, and
[ocaml-annot](http://github.com/avsm/ocaml-annot) may also be useful.

#### Eclipse

Eclipse is a popular IDE usually used for Java development. The OCaml
Development Tools (ODT) project provides equivalent IDE features for
editing and compiling OCaml code, such as automatic compilation and
name completion.

ODT is distributed as a set of plugins for the Eclipse IDE environment from the
[homepage](http://ocamldt.free.fr). You just have to copy these plugins into
your Eclipse distribution in order to access the new OCaml facilities.

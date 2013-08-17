# Installation

To work through Real World OCaml, you'll need the following software packages
installed:

* [OCaml](http://ocaml.org) version 4.1.0 or greater.  This book uses some tools that we've developed while writing it, and so you need to ensure that you have at least this version before trying the examples in the book.
* [OPAM](http://opam.ocaml.org) version 1.0 or greater.  This will give you access to all the OCaml libraries used in Real World OCaml.
* [Utop](https://github.com/diml/utop) is a modern interactive toplevel with command history, tab completion, and defaults that are tuned to work with the examples in Real World OCaml.

The easiest way to install OCaml is usually via the binary packages available
for many operating systems.  For day-to-day code development however, it's much
easier to use a source-code manager that lets you modify individual libraries
and automatically recompile all the dependencies.

An important difference between OCaml and scripting languages such as Python or
Ruby is the static type safety that means that you can't just mix-and-match
compiled libraries.  Interfaces are checked when libraries are compiled, so
when an interface is changed, all the dependent libraries must also be
recompiled.  Source-based package managers such as OPAM automate this process
for you and make development life much easier.

## Getting OCaml

The OCaml compiler is available as a binary distribution on many operating
systems.  This is the simplest and preferred installation route, but we'll also
describe how to do a manual installation as a last resort.

### Mac OS X

The [Homebrew](http://github.com/mxcl/homebrew) package manager has an
OCaml installer, which is usually updated pretty quickly to the latest stable
release.  Make sure that you have the latest XCode (and Command Line Tools for
XCode) installed from the App Store before starting the OCaml installation.

```frag
((typ console)(name installation/brew_install.out))
```

The Perl-compatible Regular Expression library (PCRE) is used by the
Core suite.  It's not strictly needed to use OCaml, but is a commonly
used library that we're installing now to save time later.

Another popular package manager on Mac OS X is [MacPorts](http://macports.org),
which also has an OCaml port.  As with Homebrew, make sure you have XCode
installed and have followed the rest of the MacPorts installation instructions,
and then type in:

```frag
((typ console)(name installation/macports_install.out))
```

### Debian Linux

On Debian Linux, you should install OCaml via binary packages.  You'll need at
least OCaml version 3.12.1 to bootstrap OPAM, which means using Debian Wheezy
or greater.  Don't worry about getting the absolute latest version of the
compiler, as you just need one new enough to compile the OPAM package manager,
after which you'll use OPAM to manage your compiler installation.

```frag
((typ console)(name installation/debian_apt.out))
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

```frag
((typ console)(name installation/fedora_install.out))
```

The PCRE package is used by Core and is just included here for convenience
later.

### Arch Linux

Arch Linux provides OCaml 4.00.1 (or later) in the standard repositories, so
the easiest method of installation is using `pacman`:

```frag
((typ console)(name installation/arch_install.out))
```

### Windows

Windows is not currently supported by the examples in Real World
OCaml, although it is being worked on.  Until that's ready, we
recommend using a virtual machine running Debian Linux on your local
machine.

### Building from source

To install OCaml from source code, first make sure that you have a C
compilation environment (usually either `gcc` or `llvm` installed).

```frag
((typ console)(name installation/ocaml_src_install.out))
```

The final step requires administrator privilege to install in your
system directory.  You can also install it in your home directory by
passing the `prefix` option to the configuration script:

```frag
((typ console)(name installation/ocaml_user_conf.out))
```

Once the installation is completed into this custom location, you will
need to add `$HOME/my-ocaml/bin` to your `PATH`, normally by editing
the `~/.bash_profile` file.  You shouldn't really to do this unless
you have special reasons, so try to install binary packages before
trying a source installation.

<note>
<title>Note to reviewers</title>

We instruct you to install the unreleased 4.01 branch version of OCaml
in these instructions, as we take advantage of some recent additions
to the language that simplify explanations in the book.  The 4.01
release will happen before the book is released, but you may run
into "bleeding edge" bugs with the release.  Leave a comment
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
over from a clean slate.  If you're using a beta version of OPAM,
please upgrade it to at least version 1.0.0 or greater before
proceeding.

### Mac OS X

Source installation of OPAM will take a minute or so on a modern
machine.  There is a Homebrew package for the latest OPAM:

```frag
((typ console)(name installation/brew_opam_install.out))
```

And on MacPorts, install it like this:

```frag
((typ console)(name installation/macports_opam_install.out))
```

### Debian Linux

OPAM has recently been packaged for Debian and will soon be part of the unstable
distribution.  If you're on an earlier stable distribution such as `wheezy`, you
can either compile from source, or cherry-pick just the OPAM binary package from
`unstable` by:

```frag
((typ console)(name installation/debian_apt_opam.out))
```

### Ubuntu Raring

OPAM is available as a Personal Package Archive on Ubuntu Raring for both i386
and x86_64.  To install it, just run:

```frag
((typ console)(name installation/ubuntu_opam_ppa.out))
```

### Fedora and Red Hat

There is currently no RPM available for Fedora or Red Hat, so please
install OPAM via the source code instructions for the moment.

### Arch Linux

OPAM is available in the Arch User Repository (AUR) in two packages.
You'll need both `ocaml` and the `base-devel` packages installed first:

* `opam` contains the most recent stable release, and is the recommended package.
* `opam-git` builds the package from the latest upstream source, and should only be used if you are looking for a specific bleeding-edge feature.

Run these commands to install the stable OPAM package:

```frag
((typ console)(name installation/arch_opam.out))
```

### Source Installation

If the binary packages aren't available for your system, you'll need to install
the latest OPAM release from source.  You can follow the online [quick install
guide](http://opam.ocamlpro.com/doc/Quick_Install.html).

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

```frag
((typ console)(name installation/opam_init.out))
```

You only need to run this command once, and it will create the
`~/.opam` directory and sync with the latest package list from the
online OPAM database.

When the `init` command finishes, you'll see some instructions about
environment variables.  OPAM never installs files into your system
directories (which would require administrator privileges).  Instead,
it puts them into your home directory by default, and can output a set
of shell commands which configures your shell with the right `PATH`
variables so that packages will just work.

If you choose not to follow the OPAM instructions to add itself
to your shell profile, you can still configure it on-the-fly in your
current shell with just one command.

```frag
((typ console)(name installation/opam_eval.out))
```

This evaluates the results of running `opam config env` in your current shell
and sets the variables so that subsequent commands will use them.  This _only_
works with your current shell and it can be only be automated for future shells
by adding the line to your login scripts.  On Mac OS X or Debian, this is
usually the `~/.bash_profile` file if you're using the default shell.  If
you've switched to another shell, it might be `~/.zshrc` instead.  OPAM isn't
unusual in this approach; the SSH `ssh-agent` also works similarly, so if
you're having any problems just hunt around in your configuration scripts to
see how that's being invoked.

If you answered `yes` to the auto-complete scripts question during
`opam init`, this should have all been set up for you. 
You can verify this worked by listing the available packages:

```frag
((typ console)(name installation/opam_list.out))
```

<note>
<title>Note to reviewers</title>

OPAM 1.0.0 places the login commands into your `~/.profile`
directory, which isn't always executed if your shell is `bash`.
This has been fixed in subsequent versions, but for now you'll need to manually
copy the contents of `~/.profile` over to `~/.bash_profile`.

</note>

The most important package we need to install is Core, which is the
replacement standard library that all of the examples in this book
use.  Before doing this, let's make sure you have exactly the right
compiler version you need.

```frag
((typ console)(name installation/opam_switch.out))
```

This step will take around ten or fifteen minutes on a modern machine, and will
download and install the OCaml compiler within the `~/.opam` directory).  OPAM
supports multiple compiler installations, and you'll find this very useful if
you ever decide to hack on the internals of the compiler or want to experiment
with the latest release without sacrificing your current installation.  You
only need to install this compiler once, and future updates will be much faster
as they only recompile libraries within the compiler installation.

The new compiler will be installed into `~/.opam/4.01.0beta1`
and any libraries you install for it will be tracked separately from
your system installation.  You can have any number of compilers
installed simultaneously, but only one can be active at any time.
Browse through the available compilers by running `opam switch list`.

Finally, we're ready to install the Core libraries.  Run this:

```frag
((typ console)(name installation/opam_install.out))
```

This will take about five or ten minutes to build, and will install a series of
packages.  OPAM figures out the dependencies you need automatically, but the
three packages that really matter are:

* `core` is the main, well-supported Core distribution from Jane Street.
* `core_bench` is a benchmarking library that makes it easy to test the
  performance profile of functions via a command-line interface.
* `core_extended` contains a number of experimental, but useful,
  extension libraries that are under review for inclusion in Core.  We
  use some of these in places, but much less than Core itself.
* `async` is the network programming library that we use in Part II to
  communicate with other hosts.  You can skip this for the initial
  installation until you get to Part II, if you prefer.

## Editing Environment

There's one last tool you need before getting started on the examples.
The default `ocaml` command gives us an interactive command-line to
experiment with code without compiling it.  However, it's quite a
spartan experience and so we use a more modern alternative.

```frag
((typ console)(name installation/opam_install_utop.out))
```

The <command>utop</command> package is an interactive command-line interface to OCaml
that has tab-completion, persistent history and integration with Emacs
so that you can run it within your editing environment.

Remember from earlier that OPAM never installs files directly into your system
directories, and this applies to <command>utop</command> too.  You'll find the binary in
`~/.opam/4.01.0beta1/bin`.  However, typing in <command>utop</command> from your shell
should just work, due to the `opam config env` step that configures your shell.
Don't forget to automate this as described earlier as it makes life much
easier when developing OCaml code!

### Command Line

The <command>utop</command> tool provides a convenient interactive toplevel, with full
command history, command macros and module name completion.  When you
first run <command>utop</command>, you'll find yourself at an interactive prompt with
a bar at the bottom of the screen.  The bottom bar dynamically updates as you
write text, and contains the possible names of modules or variables
that are valid at that point in the phrase you are entering.  You can
press the `<tab>` key to complete the phrase with the first choice. 

The `~/.ocamlinit` file in your home directory initialises <command>utop</command> with
common libraries and syntax extensions so you don't need to type them
in every time.  Now that you have Core installed, you should update it
to load it every time you start <command>utop</command>, by adding this to it:

```frag
((typ console)(name installation/show_ocamlinit.out))
```

If you only use Core libraries (and this will be the case for beginners who are
working their way through Real World OCaml as their first taste of the
language), then you can also open the Core module by default.  Just append this
line to the `.ocamlinit` file.

```frag
((typ ocaml)(name installation/open_core.ml))
```

When you run <command>utop</command> with these initialization rules, it should start up with
Core opened and ready to use.  If you don't open `Core.Std` by default, then
you must remember to open it before running any of the interactive examples in
the book.

### Editors

<note>
<title>Note to reviewers</title>

The instructions for editor setup are still being compiled.  If you have a
relevant tip or HOWTO, then we'd *really* appreciate you leaving a note here
with a pointer or direct instructions.

</note>

#### Emacs

Emacs users have tuareg and [Typerex](http://www.typerex.org/).

To use <command>utop</command> directly in Emacs, add the following line to your `~/.emacs` file:

```frag
((typ scheme)(name installation/emacsrc.scm))
```

You also need to make the `utop.el` file available to your Emacs installation.
The OPAM version of <command>utop</command> installs it into the `~/.opam` hierarchy, for
example in `~/.opam/system/share/emacs/site-lisp/utop.el`. You may need to
replace `system` with your current compiler switch, such as `4.01.0beta1`.

Once this successfully loads in Emacs, you can run utop by executing the
command <command>utop</command> in Emacs.  There are more details instructions at the
[utop homepage](https://github.com/diml/utop#integration-with-emacs).

#### Vim

Vim users can use the built-in style, and
[ocaml-annot](http://github.com/avsm/ocaml-annot) may also be useful.

#### Eclipse

Eclipse is a popular IDE usually used for Java development. The OCaml
Development Tools (ODT) project provides equivalent IDE features for
editing and compiling OCaml code, such as automatic compilation and
name completion.

ODT is distributed as a set of plugins for the Eclipse IDE environment from the
[homepage](http://ocamldt.free.fr). You just have to copy these plugins into
your Eclipse distribution in order to access the new OCaml facilities.

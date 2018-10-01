# Command-Line Parsing {#command-line-parsing}

Many of the OCaml programs that you'll write will end up as binaries that
need to be run from a command prompt. Any nontrivial command line should
support a collection of basic features:

- Parsing of command-line arguments

- Generation of error messages in response to incorrect inputs

- Help for all the available options

- Interactive autocompletion

It's tedious and error-prone to code all of this manually for every program
you write. Core provides the Command library, which simplifies all of this by
letting you declare your command-line options in one place and by deriving
all of the above functionality from these declarations. [command-line
parsing/Command library for]{.idx}

Command is simple to use for simple applications but also scales well as your
needs grow more complex. In particular, Command provides a sophisticated
subcommand mode that groups related commands together as the complexity of
your user interface grows. You may already be familiar with this command-line
style from the Git or Mercurial version control systems.

In this chapter, we'll:

- Learn how to use Command to construct basic and grouped command-line
  interfaces

- We will build simple equivalents to the cryptographic `md5` and `shasum`
  utilities

- Demonstrate how to declare complex command-line interfaces in a type-safe
  and elegant way [combinators/functional combinators]{.idx}

## Basic Command-Line Parsing {#basic-command-line-parsing}

Let's start by working through a clone of the `md5sum` command that is
present on most Linux installations (the equivalent command on Mac OS X is
simply `md5`). The following function defined below reads in the contents of
a file, applies the MD5 one-way cryptographic hash function to the data, and
outputs an ASCII hex representation of the result: [MD5 one-way cryptographic
hash function]{.idx}[command-line parsing/basic approach to]{.idx}

<link rel="import" href="code/command-line-parsing/md5/md5.ml" />

The `do_hash` function accepts a `filename` parameter and prints the
human-readable MD5 string to the console standard output. The first step
toward turning this function into a command-line program is to create a
parser for the command line arguments. The module `Command.Param` provides a
set of combinators that can be combined together to define a parameter parser
for optional flags and positional arguments, including documentation, the
types they should map to, and whether to take special actions such as pausing
for interactive input if certain inputs are encountered.

### Defining an anonymous argument {#anonymous-arguments}

Let's build a parser for a command line UI with a single *anonymous*
argument, i.e., an argument that is passed in without a flag.

<link rel="import" href="code/command-line-parsing/md5/md5.ml" part="1" />

Here, `anon` is used to signal the parsing of an anonymous argument, and the
expression `("filename" %: string)` indicates the textual name of the
argument and specification that describes the kind of value that is expected.
The textual name is used for generating help text, and the specification is
used both to nail down the OCaml type of the returned value (`string`, in
this case) and to guide features like input validation. The values `anon`,
`string` and `%:` all come from the `Command.Param` module.

### Defining basic commands {#defining-basic-commands}

Once we've defined a specification, we need to put it to work on real input.
The simplest way is to directly create a command-line interface with
`Command.basic`. [Command.basic]{.idx}

<link rel="import" href="code/command-line-parsing/md5/md5.ml" part="2" />

The `summary` argument is a one-line description which goes at the top of the
help screen, while the (optional) `readme` argument is for providing a more
detailed description that will be provided on demand.

The final argument is the most interesting one, which is the parameter
parser. This will be easier to understand if we first learn a bit more about
the type signatures of the various components we've been using. Let's do that
by recreating some of this code in the toplevel.

<link rel="import" href="code/command-line-parsing/main.mlt" part="1" />

The type parameter of `filename_param` is there to indicate the type of the
value returned by the parser; in this case, `string`.

But `Command.basic` requires a parameter parser that returns a value of type
`unit -> unit`. We can see that by using `#show` to explore the types.

<link rel="import" href="code/command-line-parsing/main.mlt" part="2" />

Note that the `'result` parameter of the type alias `basic_command` is
instantiated as `unit` for the type of `Command.basic`.

It makes sense that `Command.basic` wants a parser that returns a function;
after all, in the end, it needs a function it can run that constitutes the
execution of the program. But how do we get such a parser, given the parser
we have returns just a filename?

The answer is to use a `map` function to change the value returned by the
parser. As you can see below, the type of `Command.Param.map` is very similar
to the code of `List.map`.

<link rel="import" href="code/command-line-parsing/main.mlt" part="3" />

In our program, we used `map` to convert the `filename_param` parser, which
returns a string representing the file name, into a parser that returns a
function of type `unit -> unit` containing the body of the command.

### Running commands {#running-basic-commands}

Once we've defined the basic command, running it is just one function call
away.

<link rel="import" href="code/command-line-parsing/md5/md5.ml" part="3" />

`Command.run` takes a couple of optional arguments that are useful to
identify which version of the binary you are running in production. You'll
need to install Cryptokit via `opam install cryptokit` before building this
example. Once that's completed, run the following to compile the binary.

<link rel="import" href="code/command-line-parsing/md5/dune" />

<link rel="import" href="code/command-line-parsing/md5/md5.sh" part="build" />

You can now query the version information for the binary you just compiled:

<link rel="import" href="code/command-line-parsing/md5/md5.sh" part="get version" />

The versions that you see in the output were defined via the optional
arguments to `Command.run`. You can leave these blank in your own programs or
get your build system to generate them directly from your version control
system (e.g., by running `hg id` to generate a build revision number, in the
case of Mercurial).

We can invoke our binary with `-help` to see the auto-generated help.

<link rel="import" href="code/command-line-parsing/md5/md5.sh" part="get help" />

If you supply the `filename` argument, then `do_hash` is called with the
argument and the MD5 output is displayed to the standard output.

<link rel="import" href="code/command-line-parsing/md5/md5.sh" part="run" />

And that's all it took to build our little MD5 utility! Here's a complete
version of the example we just walked through, made slightly more succinct by
removing intermediate variables.

<link rel="import" href="code/command-line-parsing/md5_succinct/md5.ml" />

### Multi-argument commands {#multiple-arguments}

All the examples thus far have involved a single argument, but we can of
course create multi-argument commands as well. We can make a parser for
multiple arguments by binding together simpler parsers, using the function
`Command.Param.both`. Here is its type.

<link rel="import" href="code/command-line-parsing/main.mlt" part="4" />

`both` allows us to take two parameter parsers and combine them into a single
parser that returns the two arguments as a pair. In the following, we rewrite
our `md5` program so it takes two anonymous arguments: the first is an
integer saying how many characters of the hash to print out, and the second
is the filename.

<link rel="import" href="code/command-line-parsing/md5_multiarg/md5.ml" />

Building and running this command, we can see that it now indeed expects two
arguments.

<link rel="import" href="code/command-line-parsing/md5_multiarg/md5.sh" />

This works well enough for two parameters, but if you want longer parameter
lists, this approach gets old fast. A better way is to use let-syntax, which
was discussed in
[Error Handling](error-handling.html#bind-and-other-error-handling-idioms){data-type=xref}.

<link rel="import" href="code/command-line-parsing/md5_let_syntax/md5.ml" part=
"1" />

Here, we take advantage of let-syntax's support for parallel let bindings,
using `and` to join the definitions together. This syntax translates down to
the same pattern based on `both` that we showed above, but it's easier to
read and use, and scales better to more arguments.

The need to open both modules is a little awkward, and the `Param` module in
particular you really only need on the right-hand-side of the equals-sign.
This is achieved automatically by using the `let%map_open` syntax,
demonstrated below.

<link rel="import" href="code/command-line-parsing/md5_let_syntax2/md5.ml" part=
"1" />

Let-syntax is the most common way of writing parsers for `Command`, and we'll
use that idiom from here on.

Now that we have the basics in place, the rest of the chapter will examine
some of the more advanced features of Command.


## Argument Types {#argument-types}

You aren't just limited to parsing command lines of strings and ints.
`Command.Param` defines several other conversion functions (shown in
[Table14_1](command-line-parsing.html#table14_1){data-type=xref}) that
validate and parse input into various types. [arguments/argument
types]{.idx}[command-line parsing/argument types]{.idx}

::: {#table14_1 data-type=table}
Argument type | OCaml type | Example
--------------|------------|--------
`string` | `string` | `foo`
`int` | `int` | `123`
`float` | `float` | `123.01`
`bool` | `bool` | `true`
`date` | `Date.t` | `2013-12-25`
`time_span` | `Span.t` | `5s`
`file` | `string` | `/etc/passwd`

Table:  Conversion functions defined in `Command.Param`
:::



We can tighten up the specification of the command to `file` to reflect that
the argument must be a valid filename, and not just any string.

<link rel="import" href="code/command-line-parsing/md5_as_filename/md5.ml" part=
"1" />

This doesn't change the validation of the provided value, but it does enable
interactive command-line completion. We'll explain how to enable that later
in the chapter.

### Defining Custom Argument Types {#defining-custom-argument-types}

We can also define our own argument types if the predefined ones aren't
sufficient. For instance, let's make a `regular_file` argument type that
ensures that the input file isn't a character device or some other odd UNIX
file type that can't be fully read. [arguments/defining custom types]{.idx}

<link rel="import" href="code/command-line-parsing/md5_with_custom_arg/md5.ml" />

The `regular_file` function transforms a `filename` string parameter into the
same string but first checks that the file exists and is a regular file type.
When you build and run this code, you will see the new error messages if you
try to open a special device such as `/dev/null`:

<link rel="import" href="code/command-line-parsing/md5_with_custom_arg/run.errsh" />

### Optional and Default Arguments {#optional-and-default-arguments}

A more realistic MD5 binary could also read from the standard input if a
`filename` isn't specified. To do this, we need to declare the filename
argument as optional, which we can do with the `maybe` operator.
[arguments/default arguments]{.idx}[default arguments]{.idx}[optional
arguments/and default arguments]{.idx}[arguments/optional arguments]{.idx}

<link rel="import" href="code/command-line-parsing/md5_with_optional_file_broken/md5.ml" part=
"1" />

But building this results in a compile-time error.

<link rel="import" href="code/command-line-parsing/md5_with_optional_file_broken/build.errsh" />

This is because changing the argument type has also changed the type of the
value that is returned by the parser. It now produces a `string option`
instead of a `string`, reflecting the optionality of the argument. We can
adapt our example to use the new information and read from standard input if
no file is specified.

<link rel="import" href="code/command-line-parsing/md5_with_optional_file/md5.ml" />

The `filename` parameter to `do_hash` is now a `string option` type. This is
resolved into an input channel via `get_inchan` to determine whether to open
the standard input or a file, and then the rest of the command is similar to
our previous examples.

<link rel="import" href="code/command-line-parsing/md5_with_optional_file/md5.sh" />

Another possible way to handle this would be to supply a dash as the default
filename if one isn't specified. The `maybe_with_default` function can do
just this, with the benefit of not having to change the callback parameter
type.

The following example behaves exactly the same as the previous example, but
replaces `maybe` with `maybe_with_default`:

<link rel="import" href="code/command-line-parsing/md5_with_default_file/md5.ml" />

Building and running this confirms that it has the same behavior as before.

<link rel="import" href="code/command-line-parsing/md5_with_default_file/md5.sh" />

### Sequences of Arguments {#sequences-of-arguments}

Another common way of parsing anonymous arguments is as a variable length
list. As an example, let's modify our MD5 code to take a collection of files
to process on the command line. [arguments/sequences of]{.idx}

<link rel="import" href="code/command-line-parsing/md5_sequence/md5.ml" />

The callback function is a little more complex now, to handle the extra
options. The `files` are now a `string list`, and an empty list reverts to
using standard input, just as our previous `maybe` and `maybe_with_default`
examples did. If the list of files isn't empty, then it opens up each file
and runs them through `do_hash` sequentially.

<link rel="import" href="code/command-line-parsing/md5_sequence/md5.sh" />


## Adding Labeled Flags {#adding-labeled-flags}

You aren't limited to anonymous arguments on the command line. A *flag* is a
named field that can be followed by an optional argument. These flags can
appear in any order on the command line, or multiple times, depending on how
they're declared in the specification. [flags]{.idx}[command-line
parsing/labeled flags and]{.idx}

Let's add two arguments to our `md5` command that mimics the Mac OS X
version. A `-s` flag specifies the string to be hashed directly on the
command line and `-t` runs a self-test. The complete example follows.

<link rel="import" href="code/command-line-parsing/md5_with_flags/md5.ml" />

The specification now uses the `flag` function to define the two new labeled,
command-line arguments. The `doc` string is formatted so that the first word
is the short name that appears in the usage text, with the remainder being
the full help text. Notice that the `-t` flag has no argument, and so we
prepend its `doc` text with a blank space. The help text for the preceding
code looks like this:

<link rel="import" href="code/command-line-parsing/md5_with_flags/md5.sh" part=
"run" />

The `-s` flag in our specification requires a `string` argument and isn't
optional. The Command parser outputs an error message if the flag isn't
supplied, as with the anonymous arguments in earlier examples.
[Table14 2](command-line-parsing.html#table14-2){data-type=xref} contains
a list of some of the functions that you can wrap flags in to control how
they are parsed. [flag functions]{.idx}

::: {#table14-2 data-type=table}
Flag function | OCaml type
--------------|-----------
`required` *arg* | *arg* and error if not present
`optional` *arg* | *arg* `option`
`optional_with_default` *val* *arg* | *arg* with default *val* if not present
`listed` *arg* | *arg* `list`, flag may appear multiple times
`no_arg` | `bool` that is true if flag is present

Table:  Flag functions
:::



The flags affect the type of the callback function in exactly the same way as
anonymous arguments do. This lets you change the specification and ensure
that all the callback functions are updated appropriately, without runtime
errors.

## Grouping Subcommands Together {#grouping-sub-commands-together}

You can get pretty far by using flags and anonymous arguments to assemble
complex, command-line interfaces. After a while, though, too many options can
make the program very confusing for newcomers to your application. One way to
solve this is by grouping common operations together and adding some
hierarchy to the command-line interface. [subcommands, grouping
of]{.idx}[OPAM package manager]{.idx}[command-line parsing/subcommand
grouping]{.idx}

You'll have run across this style already when using the OPAM package manager
(or, in the non-OCaml world, the Git or Mercurial commands). OPAM exposes
commands in this form:

<link rel="import" href="code/command-line-parsing/opam.rawsh" />

The `config`, `remote`, and `install` keywords form a logical grouping of
commands that factor out a set of flags and arguments. This lets you prevent
flags that are specific to a particular subcommand from leaking into the
general configuration space. [install keyword]{.idx}[remote keyword]{.idx}

This usually only becomes a concern when your application organically grows
features. Luckily, it's simple to extend your application to do this in
Command: just use `Command.group`, which lets you merge a collection of
`Command.t`'s into one. [Command.group]{.idx}

<link rel="import" href="code/command-line-parsing/main.mlt" part="g.1" />

The `group` signature accepts a list of basic `Command.t` values and their
corresponding names. When executed, it looks for the appropriate subcommand
from the name list, and dispatches it to the right command handler.

Let's build the outline of a calendar tool that does a few operations over
dates from the command line. We first need to define a command that adds days
to an input date and prints the resulting date:

<link rel="import" href="code/command-line-parsing/cal_add_days/cal.ml" />

Everything in this command should be familiar to you by now, and it works as
you might expect.

<link rel="import" href="code/command-line-parsing/cal_add_days/cal.sh" />

Now, let's also add the ability to take the difference between two dates,
but, instead of creating a new binary, we'll group both operations as
subcommands using `Command.group`.

<link rel="import" href="code/command-line-parsing/cal_add_sub_days/cal.ml" />

And that's all you really need to add subcommand support! Let's build the
example first in the usual way and inspect the help output, which now
reflects the subcommands we just added.

<link rel="import" href="code/command-line-parsing/cal_add_sub_days/dune" />

<link rel="import" href="code/command-line-parsing/cal_add_sub_days/cal.sh" part=
"build" />

We can invoke the two commands we just defined to verify that they work and
see the date parsing in action:

<link rel="import" href="code/command-line-parsing/cal_add_sub_days/cal.sh" part=
"run" />

## Prompting for Interactive Input {#prompting-for-interactive-input}

Sometimes, if a value isn't provided on the command line, you want to prompt
for it instead. Let's return to the calendar tool we built before.
[interactive input/prompts for]{.idx}

<link rel="import" href="code/command-line-parsing/cal_add_days/cal.ml" />

This program requires you to specify both the `base` date and the number of
`days` to add onto it. If `days` isn't supplied on the command line, an error
is output. Now let's modify it to interactively prompt for a number of days
if only the `base` date is supplied.

<link rel="import" href="code/command-line-parsing/cal_add_interactive/cal.ml" />

The `days` anonymous argument is now an optional integer in the spec, and
when it isn't there, we simply prompt for the value as part of the ordinary
execution of our program.

Sometimes, it's convenient to pack the prompting behavior into the parser
itself. For one thing, this would allow you to easily share the prompting
behavior among multiple commands. This is easy enough to do by adding a new
function, `anon_prompt`, which creates a parser that automatically prompts if
the value isn't provided.

<link rel="import" href="code/command-line-parsing/cal_add_interactive2/cal.ml" part=
"1" />

We can see the prompting behavior if we run the program without providing the
second argument.

<link rel="import" href="code/command-line-parsing/cal_add_interactive2/cal.sh" />

## Command-Line Autocompletion with bash {#command-line-auto-completion-with-bash}

Modern UNIX shells usually have a tab-completion feature to interactively
help you figure out how to build a command line. These work by pressing the
Tab key in the middle of typing a command, and seeing the options that pop
up. You've probably used this most often to find the files in the current
directory, but it can actually be extended for other parts of the command,
too. [tab-autocompletion]{.idx}[autocompletion]{.idx}[command-line
parsing/autocompletion with bash]{.idx}

The precise mechanism for autocompletion varies depending on what shell you
are using, but we'll assume you are using the most common one: `bash`. This
is the default interactive shell on most Linux distributions and Mac OS X,
but you may need to switch to it on *BSD or Windows (when using Cygwin). The
rest of this section assumes that you're using `bash`. [bash
autocompletion]{.idx}

Bash autocompletion isn't always installed by default, so check your OS
package manager to see if you have it available.

Operating system | Package manager | Package
-----------------|-----------------|--------
Debian Linux | `apt` | `bash-completion`
Mac OS X | Homebrew | `bash-completion`
FreeBSD | Ports system | <em class="filename">/usr/ports/shells/bash-completion</em>



Once *bash* completion is installed and configured, check that it works by
typing the `ssh` command and pressing the Tab key. This should show you the
list of known hosts from your *~/.ssh/known_hosts* file. If it lists some
hosts that you've recently connected to, you can continue on. If it lists the
files in your current directory instead, then check your OS documentation to
configure completion correctly.

One last bit of information you'll need to find is the location of the
<em class="filename">bash_completion.d</em> directory. This is where all the
shell fragments that contain the completion logic are held. On Linux, this is
often in <em class="filename">/etc/bash_completion.d</em>, and in Homebrew on
Mac OS X, it would be
<em class="filename">/usr/local/etc/bash_completion.d</em> by default.

### Generating Completion Fragments from Command {#generating-completion-fragments-from-command}

The Command library has a declarative description of all the possible valid
options, and it can use this information to generate a shell script that
provides completion support for that command. To generate the fragment, just
run the command with the `COMMAND_OUTPUT_INSTALLATION_BASH` environment
variable set to any value.

For example, let's try it on our MD5 example from earlier, assuming that the
binary is called `md5` in the current directory:

<link rel="import" href="code/command-line-parsing/md5_with_flags/md5.sh" part=
"completion" />

Recall that we used the `Arg_type.file` to specify the argument type. This
also supplies the completion logic so that you can just press Tab to complete
files in your current directory.

### Installing the Completion Fragment {#installing-the-completion-fragment}

You don't need to worry about what the preceding output script actually does
(unless you have an unhealthy fascination with shell scripting internals,
that is). Instead, redirect the output to a file in your current directory
and source it into your current shell:

<link rel="import" href="code/command-line-parsing/cal_completion.rawsh" />

Command completion support works for flags and grouped commands and is very
useful when building larger command-line interfaces. Don't forget to install
the shell fragment into your global
<em class="filename">bash_completion.d</em> directory if you want it to be
loaded in all of your login shells. [completion handlers]{.idx}

::: {data-type=note}
#### Installing a Generic Completion Handler

Sadly, `bash` doesn't support installing a generic handler for all
Command-based applications. This means you have to install the completion
script for every application, but you should be able to automate this in the
build and packaging system for your application.

It will help to check out how other applications install tab-completion
scripts and follow their lead, as the details are very OS-specific.
:::



## Alternative Command-Line Parsers {#alternative-command-line-parsers}

This rounds up our tour of the Command library. This isn't the only way to
parse command-line arguments of course; there are several alternatives
available on OPAM. Three of the most prominent ones follow:
[Cmdliner]{.idx}[OCaml toolchain/ocaml-getopt]{.idx}[Arg
module]{.idx}[command-line parsing/alternatives to Command
library]{.idx}[OPAM package manager]{.idx}

The `Arg` module
: The `Arg` module is from the OCaml standard library, which is used by the
  compiler itself to handle its command-line interface. Command is generally
  more featureful than Arg (mainly via support for subcommands, the `step`
  combinator to transform inputs, and help generation), but there's
  absolutely nothing wrong with using Arg either. You can use the
  `Command.Spec.flags_of_args_exn` function to convert Arg specifications
  into ones compatible with Command. This is quite often used to help port
  older non-Core code into the Core standard library world.

[ocaml-getopt](https://forge.ocamlcore.org/projects/ocaml-getopt/)
: `ocaml-getopt` provides the general command-line syntax of GNU `getopt` and
  `getopt_long`. The GNU conventions are widely used in the open source
  world, and this library lets your OCaml programs obey the same rules.

[Cmdliner](http://erratique.ch/software/cmdliner)
: Cmdliner is a mix between the Command and Getopt libraries. It allows for
  the declarative definition of command-line interfaces but exposes a more
  `getopt`-like interface. It also automates the generation of UNIX man pages
  as part of the specification. Cmdliner is the parser used by OPAM to manage
  its command line.

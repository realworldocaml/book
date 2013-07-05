# Command Line Parsing

Many of the OCaml programs that you'll write will end up as binaries that need
to be run from a command prompt.  Any non-trivial command-line program needs a
few features:

* program options and file inputs need to be parsed from the command
  line arguments.
* sensible error messages have to be generated in response to
  incorrect inputs.
* help needs to be shown for all the available options.
* interactive auto-completion of commands to assist the user.

It's tedious and error-prone to code all this manually for every program you
write. Core provides the Command library that simplifies all this by letting
you declare all your command-line options in one place.  Command takes care of
parsing the arguments, generating help text and provides interactive
auto-completion to the user of the library.

Command also copes as you add more features to your programs.  It's a simple
library to use for small applications, and provides a sophisticated subcommand
mode that grouping related commands together as the number of options grow.
You may already be familiar with this command-line style from the Git or
Mercurial version control systems.

In this chapter, we'll:

* learn how to use Command to construct basic and grouped command-line interfaces.
* read examples that extend the cryptographic utility from [xref](#classes) and build a simple equivalent to the `md5` and `shasum` utilities.
* demonstrate how _functional combinators_ can be used to declare complex data structures in a type-safe and elegant way.

## Basic command-line parsing

Let's start by cloning the `md5` binary that is present on most Linux and Mac
OS X installations. It reads in the contents of a file, applies the MD5 one-way
cryptographic hash function to the data, and outputs an ASCII hex
representation of the result.

```frag
((typ ocaml)(name command-line-parsing/basic_md5.ml))
```

Use `opam install cryptokit` to install Cryptokit if you didn't do so earlier,
and then build the example.

```frag
((typ ocaml)(name command-line-parsing/build_basic_md5.out))
```

The `do_hash` function accepts a filename parameter and prints the
human-readable MD5 string to the console standard output.

The subsequent `command` variable defines how to invoke `do_hash` by parsing
the command-line arguments.  When you compile and run this program, it already
defines a number of useful default command-line options.

For instance, query the version information for the binary you just compiled.

```frag
((typ console)(name command-line-parsing/get_basic_md5_version.out))
```

The actual versions are defined via optional arguments to `Command.run`.  You
can leave these blank or get your build system to generate them directly from
your version control system (e.g. by running `hg tip` to generate a build
revision number, in the case of Mercurial).

```frag
((typ console)(name command-line-parsing/get_basic_md5_help.out))
```

When we invoke this binary without any arguments, it helpfully displays a help
screen that informs you that a required argument `filename` is missing.

If you do supply the filename argument, then `do_hash` is called with the
argument and the MD5 output is displayed to the standard output.

```frag
((typ console)(name command-line-parsing/run_basic_md5.out))
```

## Defining parsing specifications

So how does all this work?  Most of the interesting logic lies in how the
specifications are constructed.

The `Command.Spec` module defines several combinators that can be chained
together to define flags and anonymous arguments, what types they should map
to, and whether to take special actions (such as interactive input) if certain
fields are encountered.

### Anonymous arguments

Let's build the specification for a single argument that is specified
directly on the command-line (this is known as an _anonymous_ argument).

```ocaml
Command.Spec.(
  empty
  +> anon ("filename" %: string)
)
```

The specification above begins with an `empty` value and then adds more
parameters via the `+>` combinator.  Our example uses the `anon` function to
define a single anonymous parameter.

Anonymous parameters are created using the `%:` operator, which binds a textual
string name (used in the help text) to an OCaml conversion function.  The
conversion function is responsible for parsing the command-line fragment into
an OCaml data type.  In the example above, this is just a `string`, but we'll
see more complex conversion options below.

### Callback functions

This specification is usually combined with a callback function that accepts
all the command-line parameters as its function arguments.  Multiple arguments
are passed to the callback in the same order as they appeared in the
specification (using the `+>` operator).

The callback function is where all the actual work happens after the
command-line parsing is complete.  This function is applied with the arguments
containing the parsed command-line arguments, and takes over as the main thread
of the application.

In our example, we had just one anonymous argument, so the callback function
just has a single `string` parameter applied to it:

```ocaml
(fun file () -> do_hash file)
```

<note>
<title>The extra `unit` argument to callbacks</title>

The callback above needs an extra `unit` argument after `file`.  This is to
ensure that specifications can work even when they are empty (i.e. the
`Command.Spec.empty` value).

Every OCaml function needs at least one argument, so the final `unit`
guarantees that it will not be evaluated immediately as a value if there are no
other arguments.

</note>

### Creating basic commands

The specification and the function callback are glued together using the
`basic` command.  Let's see how this looks in our `md5` command.

```ocaml
Command.basic
  ~summary:"Generate an MD5 hash of the input data"
  ~readme:(fun () -> "More detailed information")
  Command.Spec.(
    empty
    +> anon ("filename" %: string)
  )
  (fun filename () -> do_hash filename)
```

The `basic` function takes the following arguments:

* `summary` is a required one-line description to go at the top of the command
  help screen.
* `readme` is for longer help text when the command is called with `-help`.
  The `readme` argument is a function that is only evaluated when the help
  text is actually needed.
* The specification and the callback function follow, with the arguments to
  the callback matching the order in which the specification binds
  arguments. 

### Command argument types

You aren't just limited to parsing command lines as strings, of course.  The
`Command.Spec` module defines several other conversion functions that validate
and parse input into various types:

Argument type    OCaml type    Example
-------------    -----------   -------
`string`         `string`      `foo`
`int`            `int`         `123`
`float`          `float`       `123.01`
`bool`           `bool`        `true`
`date`           `Date.t`      `2013-12-25`
`time_span`      `Span.t`      `5s`
`file`           `string`      `/etc/passwd`

A more realistic `md5` function might also read from the standard input if a
filename isn't specified.  We can change our specification with a single line
to reflect this by writing:

```ocaml
Command.Spec.(
  empty
  +> anon (maybe ("filename" %: string))
)
```

The anonymous parameter has been prefixed with a `maybe` that
indicates the value is now optional.  If you compile the example,
you'll get a type error though:

```
File "md5_broken.ml", line 18, characters 26-30:
Error: This expression has type string option
       but an expression was expected of type string
Command exited with code 2.
```

This is because the type of the callback function has changed.  It now
wants a `string option` instead of a `string` since the value is
optional.  We can quickly adapt our example to use the new information
and read from standard input if no file is specified.

```ocaml
(* md5.ml : calculate md5 with an optional filename *)
open Core.Std

let get_file_data = function
  | None
  | Some "-" -> In_channel.(input_all stdin)
  | Some file -> In_channel.read_all file

let do_hash file =
  let open Cryptokit in
  get_file_data file
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    Command.Spec.(
      empty
      +> anon (maybe ("filename" %: string))
    )
  (fun file () -> do_hash file)

let () = Command.run command
```

There are several other transformations you can do on anonymous
arguments.  We've shown you `maybe`, and you can also obtain lists of
arguments or supply default values.  Try altering the example above to
take a list of files and output checksums for all of them, just as the
`md5` command does.

Anonymous argument   OCaml type
------------------   ----------
sequence             `list` of arguments
maybe                `option` argument
maybe_with_default   argument with a default value if argument is missing

<note>
<title>Defining your own argument types</title>

You can also define your own argument parsers by using the 
`Spec.Arg_type` module directly.  For instance, the `time_span`
converter is defined as follows.

```ocaml
# open Command.Spec ;;
# let time_span = Command.Spec.Arg_type.create Time.Span.of_string;;
val time_span : Core.Span.t Command.Spec.Arg_type.t = <abstr>
# Command.Spec.(empty +> anon ("span" %: time_span));;
- : (Core.Span.t -> '_a, '_a) Command.Spec.t = <abstr>
```

</note>

## Adding flags to label the command line

You aren't just limited to anonymous arguments on the command-line.  A *flag*
is a named field that can be followed by an optional argument.  These flags can
appear in any order on the command-line, or multiple times, depending on how
they're declared in the specification.

Let's add two arguments to our `md5` command that mimics the Linux version. A
`-s` flag specifies the string to be hashed directly on the command-line and a
`-t` runs a self-test.  The complete example is:

```ocaml
(* mlmd5.ml : generate an MD5 hash of the input data *)
open Core.Std

let get_file_data file checksum =
  match file, checksum with
  | None, Some buf -> buf
  | _, Some buf -> eprintf "Warning: ignoring file\n"; buf
  | (None|Some "-"), None -> In_channel.(input_all stdin)
  | Some file, None -> In_channel.read_all file

let do_hash file checksum =
  let open Cryptokit in
  get_file_data file checksum
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    Command.Spec.(
      empty
      +> flag "-s" (optional string) ~doc:"string Checksum the given string"
      +> flag "-t" no_arg ~doc:" run a built-in time trial"
      +> anon (maybe ("filename" %: string))
    )
  (fun checksum trial file () ->
    match trial with
    | true -> printf "Running time trial\n"
    | false -> do_hash file checksum)

let () = Command.run command
```

The specification now uses the `flag` command.  The first argument to `flag` is
its name on the command-line, and the `doc` argument supplies the help text.

The `doc` string is formatted so that the first word is the short name that
appears in the usage text, with the remainder being the full help text.  Notice
that the `-t` flag has no argument, and so we prepend its `doc` text with a
blank space.  The help text for the above code looks like this:

```
$ ./mlmd5 -help
Generate an MD5 hash of the input data

  ./mlmd5 [filename]

=== flags ===

  [-s string]    Checksum the given string
  [-t]           run a built-in time trial
  [-build-info]  print info about this build and exit
  [-version]     print the version of this build and exit
  [-help]        print this help text and exit
                 (alias: -?)

$ ./mlmd5 -s "ocaml rocks"
5a118fe92ac3b6c7854c595ecf6419cb
```

The `-s` flag in our specification requires a `string` argument, and the parser
outputs an error message if it isn't supplied.  Here's a list of some of the
functions that you can wrap flags in to control how they are parsed:

Flag function            OCaml type
-------------            ----------
`required` _arg_         _arg_ and error if not present
`optional` _arg_         _arg_ `option`
`optional_with_default`  _arg_ with a default if not present
`listed` _arg_           _arg_ `list`, flag may appear multiple times
`no_arg`                 `bool` that is true if flag is present.

The flags affect the type of the callback function in exactly the same way as
anonymous arguments do.  This lets you change the specification and ensure
that all the callback functions are updated appropriately, without runtime
errors.

Notice that the `get_file_data` function now pattern matches across the
`checksum` flag and the `file` anonymous argument.  It selects the flag in
preference to the anonymous argument, but emits a warning if there's ambiguity
and both are specified.

## Grouping sub-commands together

You can get pretty far by combining flags and anonymous arguments to
assemble complex command-line interfaces.  After a while though, too
many options can make the program very confusing for newcomers to your
application.  One way to solve this is by grouping common operations
together and adding some hierarchy to the command-line interface.

You'll have run across this style already when using the OPAM package
manager (or, in the non-OCaml world, the Git or Mercurial commands).
OPAM exposes commands in this form:

```
opam config env
opam remote list -kind git
opam install --help
opam install xmlm
```

The `config`, `remote` and `install` keywords form a logical grouping
of commands, and factor out flags and arguments that are specific to
that particular operation.  It's really simple to extend your
application to do this in Command: just swap `Command.basic` for
`Command.group`:

```ocaml
val group :
  summary:string ->
  ?readme:(unit -> string) ->
  (string * t) list -> t
```

The `group` signature accepts a list of basic `Command.t` values and
their corresponding names. When executed, it looks for the appropriate
sub-command from the name list, and dispatches it to the right command
handler.

Let's build the beginning of a calendar tool that does a few
operations over dates from the command line.  We first define a
command that adds days to an input date and prints the resulting date.

```ocaml
open Core.Std

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.(
      empty
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
  (fun base span () ->
    Date.add_days base span
    |> Date.to_string
    |> print_endline
  )

let () = Command.run add
```

Once we've tested this and made sure it works, we can define another
command that takes the difference of two dates.  Both of the commands
are now grouped as sub-commands using `Command.group`.

```ocaml
open Core.Std

let add =
  Command.basic ~summary:"Add [days] to the [base] date"
    Command.Spec.(
      empty
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
  (fun base span () ->
    Date.add_days base span
    |> Date.to_string
    |> print_endline
  )

let diff =
  Command.basic ~summary:"Show days between [date1] and [date2]"
    Command.Spec.(
      empty
      +> anon ("date1" %: date)
      +> anon ("date2" %: date)
    )
  (fun date1 date2 () ->
    Date.diff date1 date2
    |> printf "%d days\n"
  )

let command =
  Command.group ~summary:"Manipulate dates"
    [ "add", add; "diff", diff ]

let () = Command.run command
```

And that's all you need to add sub-command support!  The help page for our
calendar now reflects the two commands we just added:

```
$ cal
Manipulate dates

  cal SUBCOMMAND

=== subcommands ===

  add      Add [days] to the [base] date
  diff     Show days between [date1] and [date2]
  version  print version information
  help     explain a given subcommand (perhaps recursively)

missing subcommand for command cal
```

We can invoke the two commands we just defined to verify that they work and see
the date parsing in action.

```
$ cal add 2012-12-25 40
2013-02-03
$ cal diff 2012-12-25 2012-11-01
54 days
```

## Advanced control over parsing

The use of the spec combinators has been somewhat magic so far: we just build
them up with the '+>' combinator and things seem to work.  As your programs get
larger and more complex, you'll want to factor out common functionality between
specifications.  Some other times, you'll need to interrupt the parsing to
perform special processing, such as requesting an interactive passphrase from
the user before proceeding.  We'll show you some new combinators that let you
do this now.

<sidebar>
<title>The types behind `Command.Spec`</title>

The Command module's safety relies on the specification's output values
precisely matching the callback function which invokes the main program. Any
mismatch here will inevitably result in a dynamic failure, and so Command uses
some interesting type abstraction to guarantee they remain in sync.  You don't
have to understand this section to use the more advanced combinators, but it'll
help you debug type errors as you use `Command` more.

The type of `Command.t` looks deceptively simple:

```ocaml
type ('main_in, 'main_out) t
```

You can think of `('a, 'b) t` as a function of type `'a -> 'b`, but embellished
with information about:

* how to parse the command line
* what the command does and how to call it
* how to auto-complete a partial command line

The type of a specification transforms a `'main_in` to a `'main_out` value.
For instance, a value of `Spec.t` might have type:

```ocaml
(arg1 -> ... -> argN -> 'r, 'r) Spec.t
```

Such a value transforms a main function of type `arg1 -> ... -> argN -> 'r` by
supplying all the argument values, leaving a main function that returns a value
of type `'r`.  Let's look at some examples of specs, and their types:

```ocaml
# Command.Spec.empty ;;
- : ('m, 'm) Spec.t = <abstr>
# Command.Spec.(empty +> anon ("foo" %: int)) ;;
- : (int -> '_a, '_a) Command.Spec.t = <abstr>
```

The empty specification is simple as it doesn't add any parameters to the
callback type.  The second example adds an `int` anonymous parameter that is
reflected in the inferred type.  One forms a command by combining a spec of
type `('main, unit) Spec.t` with a function of type `'main`.  The
combinators we've shown so far incrementally build the type of `'main`
according to the command-line parameters it expects, so the resulting type of
`'main` is something like ```arg1 -> ... -> argN -> unit```.

The type of `Command.basic` should make more sense now:

```ocaml
val basic :
  summary:string ->
  ?readme:(unit -> string) ->
  ('main, unit -> unit) Spec.t -> 'main -> t
```

The final line is the important one. It shows that the callback function for a
spec should consume identical arguments to the supplied `main` function, expect
for an additional `unit` argument.  This final `unit` is there to make sure the
callback is evaluated as a function, since if zero command-line arguments are
specified (i.e. `Spec.empty`), the callback would otherwise have no arguments
and be evaluated immediately.  That's why you have to supply an additional `()`
to the callback function in all the previous examples.

</sidebar>

### Composing specification fragments together

If you want to factor out common command-line operations, the `++` operator
will append two specifications together.  Let's add some dummy verbosity and
debug flags to our calendar application to illustrate this.

```ocaml
open Core.Std

let add ~common =
  Command.basic ~summary:"Add [days] to the [base] date"
    Command.Spec.(
      empty
      +> anon ("base" %: date)
      +> anon ("days" %: int)
      ++ common
    )
  (fun base span debug verbose () ->
    Date.add_days base span
    |> Date.to_string
    |> print_endline
  )

let diff ~common =
  Command.basic ~summary:"Show days between [date2] and [date1]"
    Command.Spec.(
      empty
      +> anon ("date1" %: date)
      +> anon ("date2" %: date)
      ++ common
    )
  (fun date1 date2 debug verbose () ->
    Date.diff date1 date2
    |> printf "%d days\n"
  )
```

The definitions of the specifications are very similar to the earlier
example, except that they append a `common` parameter after each
specification.  We can supply these flags when defining the groups:

```
let () =
  let common =
    Command.Spec.(
      empty
      +> flag "-d" (optional_with_default false bool) ~doc:" Debug mode"
      +> flag "-v" (optional_with_default false bool) ~doc:" Verbose output"
    )
  in
  List.map ~f:(fun (name, cmd) -> (name, cmd ~common))
    [ "add", add; "diff", diff ]
  |> Command.group ~summary:"Manipulate dates"
  |> Command.run
```

Both of these flags will now be applied and passed to all the callback
functions.  This makes code refactoring a breeze by using the compiler
to spot places where you use commands.  Just add a parameter to the
common definition, run the compiler, and fix type errors until
everything works again.

For example, if we remove the `verbose` flag above and compile, we'll
get this impressively long type error:

```ocaml
File "cal_compose_error.ml", line 39, characters 38-45:
Error: This expression has type
         (bool -> unit -> unit -> unit, unit -> unit -> unit)
         Command.Spec.t =
           (bool -> unit -> unit -> unit, unit -> unit -> unit)
           Command.Spec.t
       but an expression was expected of type
         (bool -> unit -> unit -> unit, unit -> unit) Command.Spec.t
           = (bool -> unit -> unit -> unit, unit -> unit) Command.Spec.t
       Type unit -> unit is not compatible with type unit
```

While this does look scary, the key line to scan is the last one,
where it's telling you that you have supplied too many arguments in
the callback function (`unit -> unit` vs `unit`).  If you started with
a working program and made this single change, you typically don't
even need to read the type error, as the filename and location
information is sufficient to make the obvious fix.

### Prompting for interactive input

The `step` combinator lets you control the normal course of parsing by
supplying a function that maps callback arguments to a new set of
values.  For instance, let's revisit our first calendar application
that added a number of days onto a supplied base date.

```ocaml
(* cal_add.ml *)
open Core.Std

let add_days base span () =
  Date.add_days base span
  |> Date.to_string
  |> print_endline

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.(
      empty
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
    add_days

let () = Command.run add
```

This `cal_add` program requires you to specify both the `base` date and the
number of `days` to add onto it.  If `days` isn't supplied on the command-line,
an error is output.  Now let's modify it to interactively prompt for a number
of days if only the `base` date is supplied.

```ocaml
(* cal_add_interactive.ml *)
open Core.Std

let add_days base span () =
  Date.add_days base span
  |> Date.to_string
  |> print_endline

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.( 
      step 
        (fun m base days ->
           match days with
           | Some days ->
             m base days
           | None ->
             print_endline "enter days: ";
             read_int ()
             |> m base
        )
      +> anon ("base" %: date)
      +> anon (maybe ("days" %: int))
    )
    add_days

let () = Command.run add
```

The `days` anonymous argument is now an optional integer in the spec, and we
want to transform it into a non-optional value before calling our `add_days`
callback.
The `step` combinator in the specification performs this transformation.  It
applies its supplied callback function first, which checks if `day` is defined.
If it's undefined, then it interactively reads an integer from the standard
input.  The first `m` argument to the `step` callback is the next callback
function in the chain.  The transformation is completed by calling `m base days`
to continue processing with the new values we've just calculated.
The `days` value that is passed onto the next callback now has a non-optional `int` type.

```
$ cal_add_interactive 2013-12-01
enter days:
35
2014-01-05
```

The transformation means that the `add_days` callback can just keep its
original definition of `Date.t -> int -> unit`.  The `step` function
transformed the `int option` argument from the parsing into an `int` suitable
for `add_days`.  This transformation is explicitly represented in the
type of the `step` return value:

```ocaml
# open Core.Std ;;
# open Command.Spec ;;
# step (fun m (base:Date.t) days ->
  match days with
  | Some days -> m base days
  | None ->
     print_endline "enter days: ";
     m base (read_int ()));;
- : (Date.t -> int -> '_a, Date.t -> int option -> '_a) Spec.t = <abstr>
```

The first half of the `Spec.t` shows that the callback type is `Date.t -> int`,
whereas the resulting value expected from the next specification in the
chain is a `Date.t -> int option`.

### Adding labelled arguments to callbacks

The `step` chaining lets you control the types of your callbacks very easily.
This can help you match existing interfaces or make things more explicit by
adding labelled arguments.

```ocaml
(* cal_add_labels.ml *)
open Core.Std

let add_days ~base_date ~num_days () =
  Date.add_days base_date num_days
  |> Date.to_string
  |> print_endline

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.( 
      step (fun m base days -> m ~base_date:base ~num_days:days)
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
  add_days

let () = Command.run add
```

This `cal_add_labels` example goes back to our non-interactive calendar
addition program, but the `add_days` main function now expects labelled
arguments.  The `step` function in the specification simply converts
the default `base` and `days` arguments into a labelled function,
and everything compiles again.

Labelled arguments are more verbose, but also help prevent errors with
command-line arguments with similar types but different names and purposes.
It's good form to use them when you have a lot of otherwise anonymous
`int` and `string` arguments.

## Command-line auto-completion with `bash`

Modern UNIX shells usually have a tab-completion feature to
interactively help you figure out how to build a command-line.  These
work by pressing the `<tab>` key in the middle of typing a command,
and seeing the options that pop up.  You've probably used this most
often to find the files in the current directory, but it can actually
be extended for other parts of the command too.

The precise mechanism for autocompletion varies depending on what
shell you are using, but we'll assume you are using the most common
one: `bash`.  This is the default interactive shell on most Linux
distributions and Mac OS X, but you may need to switch to it on *BSD or
Windows (when using Cygwin).  The rest of this section assumes that
you're using `bash`.

Bash autocompletion isn't always installed by default, so check your
OS package manager to see if you have it available.

Operating System  Package Manager  Package
----------------  ---------------  -------
Debian Linux      `apt`            `bash-completion`
Mac OS X          Homebrew         `bash-completion`
FreeBSD           Ports System     `/usr/ports/shells/bash-completion`

Once bash completion is installed and configured, check that it works by typing
the `ssh` command, and pressing `<tab>`.  This should show you the list of
known hosts from your `~/.ssh/known_hosts` file.  If it lists some hosts that
you've recently connected to, you can continue on.  If it lists the files in
your current directory instead, then check your OS documentation to configure
completion correctly.

One last bit of information you'll need to find is the location of the
`bash_completion.d` directory. This is where all the shell fragments that
contain the completion logic are held.  On Linux, this is often in
`/etc/bash_completion.d`, and in Homebrew on Mac OS X it would be
`/usr/local/etc/bash_completion.d` by default.

### Generating completion fragments from Command

The Command library has a declarative description of all the possible valid
options, and it can use this information to generate a shell script which
provides completion support for that command.  To generate the fragment,
just run the command with the `COMMAND_OUTPUT_INSTALLATION_BASH` environment
variable set to any value.

For example, let's try it on our calendar example from earlier, assuming that
the binary is called `cal` in the current directory:

```
$ COMMAND_OUTPUT_INSTALLATION_BASH=1 ./cal

function _jsautocom_41790 {
  export COMP_CWORD
  COMP_WORDS[0]=./cal
  COMPREPLY=($("${COMP_WORDS[@]}"))
}
complete -F _jsautocom_41790 ./cal
```

### Installing the completion fragment

You don't need to worry about what this script actually does (unless
you have an unhealthy fascination with shell scripting, that is).
Instead, redirect the output to a file in your `bash_completion.d`
directory, named after the command you're installing.

```
$ sudo env COMMAND_OUTPUT_INSTALLATION_BASH=1 ./cal \
    > /etc/bash_completion.d/cal
$ bash -l
$ ./cal <tab>
add      diff     help     version
```

The first line above redirects the earlier output into your
`bash_completion.d` directory.  The `bash -l` loads the new
configuration as a fresh login shell, and then the final line shows
the four valid commands by pressing the tab key.

Command completion support works for flags and grouped commands, and
is very useful when building larger command-line interfaces.

<note>
<title>Installing a generic completion handler</title>

Sadly, `bash` doesn't support installing a generic handler for all
Command-based applications.  This means that you have to install the
completion script for every application, but you should be able to
automate this in the build and packaging system for your application.

It will help to check out how other applications that install
tab-completion scripts and following their lead, as the details are
very OS-specific.

</note>

# Command Line Parsing

Many of the OCaml programs you will write will end up as binaries that will be
run directly from a command prompt.  Any non-trivial program invoked as a
command needs a few features:

* program options and file inputs need to be parsed from the command line arguments.
* sensible error messages have to be generated in response to incorrect inputs.
* show help and manual pages for all the available options.
* support interactive auto-completion of commands to assist the user.

Supporting all of this functionality manually is tedious and error-prone. Core
provides the `Command` library that lets you declare your command-line options
in one data structure, and the library takes care of parsing, help generation
and auto-completion.  Command is simple to use for smaller applications, and
has a sophisticated sub-command mode that groups related commands together (you
may be familiar with this style from `git` or `hg`).

This chapter demonstrates how to use `Command` to extend the cryptographic
utility from [xref](#object-oriented-programming) and build a simple equivalent
to the `md5` and `shasum` utilities. It also continues the demonstration of how
elegant _functional combinators_ can be to declare complex data structures in a
type-safe way.

## Basic command line parsing

We'll begin by cloning the `md5` binary that is present on most Linux distributions
and MacOS X.  It reads in the contents of a file, applies the MD5 one-way hash function
to the data, and outputs an ASCII hex representation of the result.

```ocaml
open Core.Std

let do_hash file =
  let open Cryptokit in
  In_channel.read_all file
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(
      empty
      +> anon ("filename" %: string)
    )
  (fun file () -> do_hash file)

let () = Command.run command
```

The `do_hash` function accepts a filename parameter and prints the human-readalbe MD5
string to the console sandard output.  We want to control the inputs to this function
via the command-line, and this is what the subsequent `command` value declares.
If you compile this program and run it, the help screen looks like this:

```
$ ./basic.byte 
Generate an MD5 hash of the input data

  basic.byte filename

More detailed information

=== flags ===

  [-build-info]  print info about this build and exit
  [-version]     print the version of this build and exit
  [-help]        print this help text and exit
                 (alias: -?)

missing anonymous argument: filename
```

If we invoke the binary without any arguments, it emits a help screen that informs you
that a required argument `filename` is missing.  Supplying the argument to the command
results in `do_hash` being called, and the MD5 output being displayed to the standard output.

```
$ ./basic.byte basic.byte
59562f5e4f790d16f1b2a095cd5de844
```

So how does all this work?  There are three parts to defining a command-line interface:

* `Command.basic` takes a function which is passed parameters parsed from the command-line according to a `spec` parameter.  It takes a
`summary` parameter for a one-line description of the command behavior, and  `readme`  for longer help text.
* `Command.spec` defines the steps required to convert a command-line into an OCaml structure.
* `Command.run` actually executes a command and its specification, and runs the callback function with the resulting parameters.

Most of the interesting logic lies in how the specifications are defined.  
The `Command.Spec` module defines several combinators that can be chained together to define flags and anonymous arguments, what types they should map to, and whether to take special actions (such as interactive input) if certain fields are encountered.

```ocaml
  Command.Spec.(
    empty
    +> anon ("filename" %: string)
  )
```

Here, we begin the specification with an `empty` value, and then chain more parameters via the `+>` combinator.
Our simple `example define a single _anonymous_ parameter via the `anon` function (that is, a standalone token from the command-line).
Anonymous functions can be assigned a name that is used in help text and an OCaml type that they are mapped to.
The parameters specified are all passed to a callback function which actually invokes the program logic.

```ocaml
(fun file () -> do_hash file)
```

In our example, the function takes a `file` parameter that is a `string`, and maps it to the `do_hash` function.  You aren't just limited to strings though, as `Command.Spec` defines several other conversion functions that validate and parse input into various types:

Argument type    OCaml type    Example
-------------    -----------   -------
`string`         `string`      `foo`
`int`            `int`         `123`
`float`          `float`       `123.01`
`bool`           `bool`        `true`
`date`           `Date.t`      `2013-12-25`
`time_span`      `Span.t`      `5s`
`file`           `string`      `<valid filename>`

Anonymous arguments don't have to be declared individually.  A more realistic `md5` function might also read from the standard input if a filename isn't specified.  We can change our specification with a single line to reflect this by writing:

```ocaml
Command.Spec.(
  empty
  +> anon (maybe ("filename" %: string))
)
```

The anonymous parameter has been prefixed with a `maybe` that indicates the value is now optional.  If you compile the example, you'll get a type error though:

```
File "basic_broken.ml", line 18, characters 26-30:
Error: This expression has type string option
       but an expression was expected of type string
Command exited with code 2.
```

This is because the type of the callback function has changed.  It now wants a `string option` instead of a `string` since the value is optional.  We can quickly adapt our example to use the new information and read from standard input if no file is specified.

```ocaml
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

There are several other transformations you can do on anonymous arguments. We've shown you `maybe`, and you can also obtain lists of arguments or supply default values.  Try altering the example above to take a list of files and output checksums for all of them,
just as the `md5` command does.

Anonymous argument   OCaml type
------------------   ----------
sequence             `list` of arguments
maybe                `option` argument
maybe_with_default   argument with a default value if argument is missing

## Using Flags

You aren't just limited to anonymous arguments on the command-line, of course.  Flags (such as `-v`) can be specified in the same manner as anonymous arguments.  These can appear in any order on the command-line, or multiple times, depending on how they're declared.
Let's add two arguments to our `md5` command that mimic the Linux version: a `-s` flag to specify the string to be hashed directly, and a `-t` self-benchmark test.

```ocaml
Command.Spec.(
  empty
  +> flag "-s" (optional string) ~doc:"string Checksum the given string"
  +> flag "-t" no_arg ~doc:" run a built-in time trial"
  +> anon (maybe ("filename" %: string))
)
```
The `flag` command is quite similar to `anon`.  The first argument is the
flag name, and aliases can be specified via an optional argument.  The `doc`
string should be formatted so that the first word is the short name that
should appear in the usage text, with the remainder being the full help text.
Notice that the `-t` flag has no argument, and so we prepend the doc text
with a blank space.
The help text for the above fragment looks like this:

```
$ mlmd5 -s 
Generate an MD5 hash of the input data

  mlmd5 [filename]

=== flags ===

  [-s string]    Checksum the given string
  [-t run]       a built-in time trial
  [-build-info]  print info about this build and exit
  [-version]     print the version of this build and exit
  [-help]        print this help text and exit
                 (alias: -?)

missing argument for flag -s

$ mlmd5 -s "ocaml rocks"
5a118fe92ac3b6c7854c595ecf6419cb
```

The `-s` flag requires a `string` argument in our specification,
and the parser outputs an error message if it isn't supplied.
Here's a list of some of the functions that you can wrap flags in
to control how they are parsed:

Flag function            OCaml type
-------------            ----------
`required` _arg_         _arg_ and error if not present
`optional` _arg_         _arg_ `option`
`optional_with_default`  _arg_ with a default if not present
`listed` _arg_           _arg_ `list` may appear multiple times
`no_arg`                 `bool` that is true if flag is present.

The flags affect the type of the callback function in exactly the
same way as anonymous arguments do.  The full example of our `md5`
function with flags is below.

```ocaml
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
      +> flag "-t" no_arg ~doc:"run a built-in time trial"
      +> anon (maybe ("filename" %: string))
    )
  (fun checksum trial file () ->
    match trial with
    | true -> printf "Running time trial\n"
    | false -> do_hash file checksum)

let () = Command.run command
```

Notice how the `get_file_data` function now pattern matches
across the `checksum` flag and the `file` anonymous argument.
It selects the flag in preference to the file argument, but
emits a warning if there's ambiguity.


Files, Modules, and Programs
============================

We've so far experienced OCaml only through the toplevel.  As you move
from exercises to real-world programs, you'll need to leave the
toplevel behind and start building programs from files.  Files are
more than just a convenient way to store and manage your code; in
OCaml, they also act as abstraction boundaries that divide your
program into conceptual components.

In this chapter, we'll show you how to build an OCaml program from a
collection of files, as well as the basics of working with modules and
module signatures.

## Single File Programs ##

We'll start with an example: a utility that reads lines from `stdin`,
computes a frequency count for each line, and then writes out the 10
lines with the highest frequency counts.  Here's a simple
implementation, which we'll save as the file `freq.ml`.  Note that
we're using several functions from the `List.Assoc` module, which
provides utility functions for interacting with association lists,
_i.e._, lists of key/value pairs.

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

(* build_counts recursively builds up a mapping from lines to
   number of occurences of that line. *)
let rec build_counts counts =
  match In_channel.input_line stdin with
  | None -> counts (* EOF, so return the counts accummulated so far *)
  | Some line ->
    (* get the number of times this line has been seen before,
       inferring 0 if the line doesn't show up in [counts] *)
    let count =
      match List.Assoc.find counts line with
      | None -> 0
      | Some x -> x
    in
    (* increment the count for line by 1, and recurse *)
    build_counts (List.Assoc.add counts line (count + 1))

let () =
  (* Compute the line counts *)
  let counts = build_counts [] in
  (* Sort the line counts in descending order of frequency *)
  let sorted_counts = List.sort ~cmp:(fun (_,x) (_,y) -> descending x y) counts  in
  (* Print out the 10 highest frequency entries *)
  List.iter (List.take 10 sorted_counts) ~f:(fun (line,count) ->
    printf "%3d: %s\n" count line)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<sidebar><title>Where is the main function?</title>

Unlike C, programs in OCaml do not have a unique `main` function. When
an OCaml program is evaluated, all the statements in the
implementation files are evaluated in order.  These implementation
files can contain arbitrary expressions, not just function
definitions. In this example, the role of the `main` function is
played by the expression `let () = process_lines []`, which kicks off
the actions of the program.  But really the entire file is evaluated
at startup, and so in some sense the full codebase is one big `main`
function.

</sidebar>

If we weren't using `Core` or any other external libraries, we could
build the executable like this:

~~~~~~~~~~~~~~~
ocamlc freq.ml -o freq
~~~~~~~~~~~~~~~

But in this case, this command will fail with the error `Unbound
module Core`.  We need a somewhat more complex invocation to get Core
linked in:

~~~~~~~~~~~~~~~
ocamlfind ocamlc -linkpkg -thread -package core freq.ml \
   -o freq
~~~~~~~~~~~~~~~

Here we're using `ocamlfind`, a tool which itself invokes other parts
of the ocaml toolchain (in this case, `ocamlc`) with the appropriate
flags to link in particular libraries and packages.  Here, `-package
core` is asking `ocamlfind` to link in the Core library, `-linkpkg` is
required to do the final linking in of packages for building a
runnable executable, and `-thread` turns on threading support, which
is required for Core.

While this works well enough for a one-file project, more complicated
builds will require a tool that can be used to orchestrate the build.
One great tool for this task is `ocamlbuild`, which is shipped with
the OCaml compiler.  We'll talk more about `ocamlbuild` in chapter
{{{OCAMLBUILD}}}, but for now, we'll just walk through the steps
required for this simple application.  First, create a `_tags` file,
containing the following lines.

~~~~~~~~~~~~~~~
true:package(core)
true:thread
~~~~~~~~~~~~~~~

The purpose of the `_tags` file is to specify which compilation
options are required for which files.  In this case, we're telling
`ocamlbuild` to link in the `core` package and to turn on threading
for all files (the pattern `true` matches every file in the
project.)

We then create a build script `build.sh` that invokes `ocamlbuild`:

~~~~~~~~~~~~~~~
#!/usr/bin/env bash

ocamlbuild -use-ocamlfind freq.byte && cp freq.byte freq
~~~~~~~~~~~~~~~

If you invoke `build.sh`, you'll get a bytecode executable.  If we'd
used a target of `unique.native` in `build.sh`, we would have gotten
native-code instead.

Whichever way you build the application, you can now run it from the
command-line.  The following line extracts strings from the `ocamlopt`
executable, and then reports the most frequently occurring ones.

~~~~~~~~~~~~~~~~~
$ strings `which ocamlopt` | ./freq
 13: movq
 10: cmpq
  8: ", &
  7: .globl
  6: addq
  6: leaq
  5: ", $
  5: .long
  5: .quad
  4: ", '
~~~~~~~~~~~~~~~~~

<sidebar><title>Byte-code vs native-code</title>

OCaml ships with two compilers---the `ocamlc` byte-code compiler, and
the `ocamlopt` native-code compiler.  Programs compiled with `ocamlc`
are interpreted by a virtual machine, while programs compiled with
`ocamlopt` are compiled to native machine code to be run on a specific
operating system and processor architecture.

While the two compilers produce programs with the same behavior, there
are some differences.  One basic tradeoff has to do with speed: the
byte-code compiler compiles faster, whereas the native-code compiler
generates faster executables.  Also, the byte-code compiler can be
used on more architectures, and has some better tool support; in
particular, the OCaml debugger only works with byte-code.


As a general matter, production executables should usually be built
using the native-code compiler, and it sometimes makes sense to use
bytecode for development builds.  And, of course, bytecode makese
sense when targetting a platform not supported by the native code
compiler.
</sidebar>


## Multi-file programs and Modules ##

Source files in OCaml are tied into the module system, with each file
compiling down into a module whose name is derived from the name of
the file.  We've encountered modules before, for example, when we used
functions like `find` and `add` from the `List.Assoc` module.  At it's
simplest, you can think of a module as a collection of definitions
that are stored within a namespace.

Let's consider how we can use modules to refactor the implementation
of `freq.ml`.  Remember that the variable `counts` contains an
association list, _i.e._, a list of key/value pairs, representing the
counts of the lines seen so far.  But updating an association list
takes time linear in the length of the list, meaning that the time
complexity of processing a file is quadratic in the number of distinct
lines in the file.

We can fix this problem by replacing association lists with a more
efficient datastructure.  We'll do that, but first we'll factor out
the key functionality into a separate module with an explicit
interface.  We can consider alternative (and more efficient)
implementations once we have a clear interface to program against.

We'll start by creating a file, `counter.ml`, that contains the logic
for maintaining the association list used to describe the counts.  The
key function, called `touch`, updates the association list with the
information that a given line should be added to the frequency counts.

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

let touch t s =
  let count =
    match List.Assoc.find t s with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count + 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now rewrite `freq.ml` to use `Counter`.  Note that the
resulting code can still be built with `build.sh`, since `ocambuild`
will discover dependencies and realize that `counter.ml` needs to be
compiled.

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

let rec build_counts counts =
  match In_channel.input_line stdin with
  | None -> counts
  | Some line -> build_counts (Counter.touch counts line)

let () =
  let counts = build_counts [] in
  let sorted_counts = List.sort counts
    ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  in
  List.iter (List.take sorted_counts 10)
    ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Signatures and Abstract Types ###

While we've pushed some of the logic to the `Counter` module, the code
in `freq.ml` can still depend on the specific types used in `Counter`.
Indeed, the invocation of `build_counts`, as shown below:

~~~~~~~~~~~~~~~~ { .ocaml }
  let counts = build_counts [] in
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

depends on the fact that the empty set of frequency counts is
represented as an empty list.  We'd like to prevent this kind of
dependency, so that we can change the implementation of `Counter`
without needing to change client code like that in `freq.ml`.

The first step towards hiding the implementation details of `Counter`
is to create an interface file, `counter.mli`, which controls how
`counter` is accessed.  Let's start by writing down the trivial
interface, _i.e._, an interface that describes what's currently
available in `Counter` without hiding anything.  We'll use `val`
declarations in the `mli`, which have the following syntax

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
val <identifier> : <type>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and are used to expose the existence of a given value in the module.
Here's an interface that describes the current contents of `Counter`.
Note that if we put these lines in `counter.mli` and compile, the
program will build as before.

~~~~~~~~~~~~~~~~ { .ocaml }
val touch : (string * int) list -> string -> (string * int) list
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can hide the fact that frequency counts are represented as
association lists by making the type of frequency counts _abstract_.
A type is abstract if its name is exposed in the interface, but its
definition is not.  Here's how you might write an abstract interface
for `Counter`:

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t

val empty : t
val touch : t -> string -> t
val to_list : t -> (string * int) list
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we needed to add `empty` and `to_list` to `Counter`, since
otherwise, there would be no way to create `Counter.t` or to get data
out of one.

Here's a rewrite of `counter.ml` to match this signature.

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t = (string * int) list

let empty = []

let touch t s =
  let count =
    match List.Assoc.find t s with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count + 1)

let to_list x = x
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we now try to compile `freq.ml`, we'll get the following error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
+ /usr/local/bin/ocamlfind ocamlc -c -annot -thread -package core -o freq.cmo freq.ml
File "freq.ml", line 11, characters 20-22:
Error: This expression has type 'a list
       but an expression was expected of type Counter.t
Command exited with code 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is because `freq.ml` depends on the fact that frequency counts
are represented as association lists, a fact that we've just hidden.
We just need to fix the code to use `Counter.empty` instead of `[]`
and `Counter.to_list` to get the association list out at the end for
processing and printing.

Now we can turn to optimizing the implementation of `Counter`.  Here's
an alternate and far more efficient implementation, based on the `Map`
datastructure in Core.

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t = (string,int) Map.t

let empty = Map.empty

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.add t s (count + 1)

let to_list t = Map.to_alist t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

## More on modules and signatures

### Concrete types in signatures

In our frequency-count example, the module `Counter` had an abstract
type `Counter.t` for representing a collection of frequency counts.
Sometimes, you'll want to make a type in your interface _concrete_, by
including the type definition in the interface.

For example, Imagine we wanted to add a function to `Counter` for
returning the line with the median frequency count, and in the case
where the number of lines is even and there is no precise median, it
returns the two lines before and after the median.  We could do this
by adding a new type to the interface to represent this possible
return value.  You might implement the function like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type median = | Median of string
              | Before_and_after of string * string

let median t =
  let sorted_strings = List.sort (Map.to_alist t)
      ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  in
  let len = List.length sorted_strings in
  let nth n = List.nth_exn sorted_strings n in
  if len mod 2 = 1
  then Median (nth (len/2))
  else Before_and_after (nth (len/2) (len/2 + 1))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, to expose this usefully in the interface, we need to expose the
definition of `median`, so that clients can pattern match on it.  We'd
do that as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type median = | Median of string
              | Before_and_after of string * string

val get_median : t -> median
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You should think carefully about whether a given type should be
abstract or concrete.  Abstract types give you more control over how
values of that type are created and accessed; concrete types let you
expose more detail and structure to client code in a lightweight way.
The right choice depends very much on the context.

### Module and signature includes ###

OCaml provides a number of tools for manipulating modules.  One
particularly useful one is the `include` directive.  The basic
functionality of `include` is to dump the contents of one module into
another.  This is useful when you want to extend an existing module.
For example, imagine you want to create a type for a URL, where the
base representation of a URL is a string.  Here's a simple interface:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t

val of_string : string -> t option
val to_string : t -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and implementation (assuming that you already have a function called

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t = string

let is_valid_url s =
   ... (* some code for validating URLs *) ...

let of_string s =
   if is_valid_url s then Some s else None

val to_string s = s
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This gives you an abstract type of strings that are guaranteed to be
valid URLs.  But it's quite incomplete in that you're missing many of
the useful functions associated with strings.  In particular, you
might want to have access to the comparison and hash functions
associated with strings.  One way of getting access to this is by
rewriting this module as an extension of the String module.  The
implementation would then look like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

include String

let is_valid_url s =
   ... (* some code for validating URLs *) ...

let of_string s =
   if is_valid_url s then Some s else None

val to_string s = s
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the interface level, you need to decide which parts of the
functionality of string to expose.  You could manually add in each
function that you wanted to expose, but a simpler approach is to use
include sub-signatures that summarize the relevant functionality in a
simple way.  In particular, you could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t
include Hashable with type t := t
include Comparable with type t := t

val of_string : string -> t option
val to_string : t -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~


_[Discuss how include works, using the example of creating a custom
string identifier]_

### Modules within a file ###

- module expressions at the top-level.  Show how the example of a
  custom identifier can be done very concisely inside of a module.
- `let module`

### Opening modules ###

One useful primitive in OCaml's module language is the `open`
directive.  We've seen that already in the `open Core.Std` that has
been at the top of our source files.

The basic purpose of `open` is to change the namespaces that OCaml
searches when trying to resolve an identifier.  Roughly, if you open a
module `M`, then every subsequent time you look for an identifier
`foo`, the module system will look in `M` for a value named `foo`.
This is true for all kinds of identifiers, including types, type
constructors, values and modules.

`open` is essential when dealing with something like a standard
library, but it's generally good style to keep opening of modules to a
minimum.  Opening a module is basically a tradeoff between terseness
and explicitness --- the more modules you open, the harder it is to
know by looking at it where any individual identifier is defined.

_[Discuss alternatives to opening modules, including local opens and
local `let module` definitions]_

### Common errors with modules

When OCaml compiles a program with an `ml` and an `mli`, it will
complain if it detects a mismatch between the two.  Here are some of
the common errors you'll run into.

#### Type mismatches

The simplest kind of error is where the type specified in the
signature does not match up with the type in the implementation of the
module.  As an example, if we replace the `val` declaration in
`counter.mli` by swapping the types of the first two arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val touch : string -> t -> t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

and then try to compile `Counter` (by writing `ocamlbuild
-use-ocamlfind counter.cmo`), we'll ge the following error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       Values do not match:
         val touch :
           ('a, int) Core.Std.Map.t -> 'a -> ('a, int) Core.Std.Map.t
       is not included in
         val touch : string -> t -> t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error message is a bit intimidating at first, and it takes a bit
of thought to see where the first type, which is the type of [touch]
in the implementation, doesn't match the second one, which is the type
of [touch] in the interface.  You need to recognize that [t] is in
fact a [Core.Std.Map.t], and the problem is that in the first type,
the first argument is a map while the second is the key to that map,
but the order is swapped in the second type.

#### Missing definitions

We might decide that we want a new function in `Counter` for pulling
out the frequency count of a given string.  We can update the `mli` by
adding the following line.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val count : t -> string -> int
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, if we try to compile without actully adding the implementation,
we'll get this error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       The field `count' is required but not provided
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A missing type definition will lead to a similar error.

#### Type definition mismatches

Imagine we wanted to add a function to `Counter` for returning the
median line, and in the case where the number of lines is even and
there is no precise median, it returns the two lines before and after
the median.  Here's one way of rendering such a call in the interface.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type median = | Exact of line
              | Surrounding of line * line

val get_median : t -> median
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the type `median` is concrete in the interface.  Note that
order of the declaration of variants matters, so, if the definition of
`median` in the implementation lists those options in a different
order:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type median = | Surrounding of line * line
              | Exact of line
~~~~~~~~~~~~~~~~~~~~~~~~~~~

that will lead to a compilation error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       Type declarations do not match:
         type median = Surrounding of string * string | Exact of string
       is not included in
         type median = Exact of string | Surrounding of string * string
       Their first fields have different names, Surrounding and Exact.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Order is similarly important in other parts of the signature,
including the order in which record fields are declared and the order
of arguments (including labelled and optional arguments) to a
function.

#### Cyclic dependencies

_[This is an ocamlbuild error, really...]_


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
  let counts = List.sort ~cmp:(fun (_,x) (_,y) -> descending x y) counts  in
  (* Print out the first 10 lines in the list *)
  List.iter (List.take counts 10) ~f:(fun (line,count) ->
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


## Files and Modules ##

Source files in OCaml are tied into the module system, with each file
compiling down into a module, whose name is derived from the name of
the file.  We've encountered modules before, for example, when we used
functions like `find` and `add` from the `List.Assoc` module.  At it's
simplest, you can think of a module as a collection of definitions
that are stored within a namespace.

Let's consider how we can use modules to refactor the implementation
of `freq.ml`.  Remember that the variable `counts` contains an
association list, _i.e._, a list of pairs of keys and values,
representing the counts of the lines seen so far.  But updating an
association list takes time linear in the length of the list, meaning
that the time complexity of processing a file is quadratic in the
number of distinct lines in the file.

To fix this problem, we'll need a more efficient datastructure than an
association list.  To do that, we'll first factor out the key
functionality into a separate module with an explicit interface.
Then, we can consider alternative implementations of that interface,
and see how they effect performance.

The functionality we'll factor out is that of keeping track of line
counts.  `counter.ml` below has just the code for maintaining a line
count, built in the same way as before on top of an association list.

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
interface, _i.e._, one that doesn't hide anything, but instead fully
describes what's currently available in `counter`.  We'll use `val`
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
A type in a module if the name of the type is exposed in the
interface, but the definition of that type is not.  In the case of
`Counter`, an abstract interface might look like this:

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type t

val empty : t
val touch : t -> string -> t
val to_list : t -> (string * int) list
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we needed to add [empty] and [to_list], since those expose
the two other pieces of functionality that the code in `freq.ml`
depends on.

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


# Points to hit

- cyclic dependencies are not allowed
- Type errors
- the perils of open
- Comments (and doc-comments?) in mli's
- more on ocamlbuild


# Detritus

## Signatures

, and that are
constrained by a _signature_, _i.e._, an interface.  The signature of
a module determines what elements of the module are visible
externally, and constrains the types of those elements.

The mapping to the filesystem is fairly simple: `.ml` files are
treated as module definitions, while `.mli` files are treated as
module signature definitions.  Files named `foo.ml` and `foo.mli` will
be compiled into a module `Foo`, whose implementation comes from the
`.ml` file, and whose signature comes from the `.mli`.


------------------------------------------------

We can now rewrite `counter.ml` in any number of ways, for example,
by using the `Set` module provided in Core:

~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

type 'a t = 'a Set.Poly.t

let empty = Set.Poly.empty
let add l x = Set.add l x
let mem l x = Set.mem l x
~~~~~~~~~~~~~~~~~~~~~~~~~~~


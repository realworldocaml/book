Files, Modules and Programs
===========================

We've so far experienced OCaml largely through the toplevel.  As you
move from exercises to real-world programs, you'll need to leave the
toplevel behind and start building programs from files.  Files are
more than just a convenient way to store and manage your code; in
OCaml, they also act as boundaries that divide your program into
conceptual units.

In this chapter, we'll show you how to build an OCaml program from a
collection of files, as well as the basics of working with modules and
module signatures.

## Single File Programs ##

We'll start with an example: a utility that reads lines from `stdin`
and computes a frequency count of the lines that have been read in.
At the end, the 10 lines with the highest frequency counts are written
out.  We'll start with a simple implementation, which we'll save as
the file `freq.ml`.

This implementation will use two functions from the `List.Assoc`
module, which provides utility functions for interacting with
association lists, _i.e._, lists of key/value pairs.  In particular,
we use the function `List.Assoc.find`, which looks up a key in an
association list, and `List.add`, which adds a new binding to an
association list, as shown below.

```ocaml
# let assoc = [("one", 1); ("two",2); ("three",3)];;
val assoc : (string * int) list = [("one", 1); ("two", 2); ("three", 3)]
# List.Assoc.find assoc "two";;
- : int option = Some 2
# List.Assoc.add assoc "four" 4;; (* add a new key *)
[("four", 4); ("one", 1); ("two", 2); ("three", 3)]
# List.Assoc.add assoc "two" 4;; (* overwrite an existing key *)
- : (string, int) List.Assoc.t = [("two", 4); ("one", 1); ("three", 3)]
```

Note that `List.Assoc.add` doesn't modify the original list, but
instead allocates a new list with the requisite key/value added.

Now we can write down `freq.ml`.

```ocaml
(* freq.ml: basic implementation *)

open Core.Std

let build_counts () =
  In_channel.fold_lines stdin ~init:[] ~f:(fun counts line ->
    let count =
      match List.Assoc.find counts line with
      | None -> 0
      | Some x -> x
    in
    List.Assoc.add counts line (count + 1)
  )

let () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,x) (_,y) -> compare y x)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
```

The function `build_counts` reads in lines from `stdin`, constructing
from those lines an association list with the frequencies of each
line.  It does this by invoking `In_channel.fold_lines` (similar to
the function `List.fold` described in [xref](#lists-and-patterns)),
which reads through the lines one by one, calling the provided fold
function for each line to update the accumulator.  That accumulator is
initialized to the empty list.

With `build_counts` defined, we then call the function to build the
association list, sort that list by frequency in descending order,
grab the first 10 elements off the list, and then iterate over those
ten elements and print them to the screen.  These operations are tied
together using the `|>` operator, as described in
[xref](#variables-and-functions).


<note><title>Where is the main function?</title>

Unlike C, programs in OCaml do not have a unique `main` function.
When an OCaml program is evaluated, all the statements in the
implementation files are evaluated in order.  These implementation
files can contain arbitrary expressions, not just function
definitions. In this example, the declaration starting with `let () =`
plays the role of the `main` declaration, kicking off the processing.
But really the entire file is evaluated at startup, and so in some
sense the full codebase is one big `main` function.

</note>

If we weren't using Core or any other external libraries, we could
build the executable like this:

```
ocamlc freq.ml -o freq
```

But in this case, this command will fail with the error `Unbound
module Core`.  We need a somewhat more complex invocation to get Core
linked in:

```
ocamlfind ocamlc -linkpkg -thread -package core freq.ml -o freq
```

Here we're using `ocamlfind`, a tool which itself invokes other parts
of the ocaml toolchain (in this case, `ocamlc`) with the appropriate
flags to link in particular libraries and packages.  Here, `-package
core` is asking `ocamlfind` to link in the Core library, `-linkpkg` is
required to do the final linking in of packages for building a
runnable executable, and `-thread` turns on threading support, which
is required for Core.

While this works well enough for a one-file project, more complicated
builds will require a tool to orchestrate the build.  One great tool
for this task is `ocamlbuild`, which is shipped with the OCaml
compiler.  We'll talk more about `ocamlbuild` in [xref](#packaging),
but for now, we'll just walk through the steps required for this
simple application.  First, create a `_tags` file containing the
following lines:

```
true:package(core),thread,annot,debugging
```

The purpose of the `_tags` file is to specify which compilation
options are required for which files.  In this case, we're telling
`ocamlbuild` to link in the `core` package and to turn on threading,
generation of annotation files, and debugging support for all files
(since the condition `true` evaluates to `true` on all files).

We can then invoke `ocamlbuild` to build the executable.

```
$ ocamlbuild -use-ocamlfind freq.byte
```

If we'd invoked `ocamlbuild` with a target of `freq.native` instead of
`freq.byte`, we would have gotten native-code instead.

We can now run the our program from the command-line.  The following
line extracts strings from the `ocamlopt` executable, reporting the
most frequently occurring ones.

```
$ strings `which ocamlopt` | ./freq.byte
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
```

<note><title>Bytecode vs native code</title>

OCaml ships with two compilers: the `ocamlc` bytecode compiler and the
`ocamlopt` native-code compiler.  Programs compiled with `ocamlc` are
interpreted by a virtual machine, while programs compiled with
`ocamlopt` are compiled to native machine code to be run on a specific
operating system and processor architecture.

Aside from performance, executables generated by the two compilers
have nearly identical behavior.  There are a few things to be aware
of.  First, the bytecode compiler can be used on more architectures,
and has some tools that are not available for native code.  For
example, the OCaml debugger only works with bytecode (although the
`gdb`, the Gnu Debugger, works with OCaml native-code applications).
The bytecode compiler is also quicker than the native-code compiler.
In addition, in order to run a bytecode executable you typically need
to have OCaml installed on the system in question.  That's not
strictly required, though, since you can build a bytecode executable
with an embedded runtime, using the `-custom` compiler flag.

As a general matter, production executables should usually be built
using the native-code compiler, but it sometimes makes sense to use
bytecode for development builds.  And, of course, bytecode makes sense
when targeting a platform not supported by the native-code compiler.

</note>


## Multi-file programs and modules ##

Source files in OCaml are tied into the module system, with each file
compiling down into a module whose name is derived from the name of
the file.  We've encountered modules before, for example, when we used
functions like `find` and `add` from the `List.Assoc` module.  At it's
simplest, you can think of a module as a collection of definitions
that are stored within a namespace.

Let's consider how we can use modules to refactor the implementation
of `freq.ml`.  Remember that the variable `counts` contains an
association list representing the counts of the lines seen so far.
But updating an association list takes time linear in the length of
the list, meaning that the time complexity of processing a file is
quadratic in the number of distinct lines in the file.

We can fix this problem by replacing association lists with a more
efficient data structure.  To do that, we'll first factor out the key
functionality into a separate module with an explicit interface.  We
can consider alternative (and more efficient) implementations once we
have a clear interface to program against.

We'll start by creating a file, `counter.ml` that contains the logic
for maintaining the association list used to describe the counts.  The
key function, called `touch`, updates the association list with the
information that a given line should be added to the frequency counts.

```ocaml
(* counter.ml: first version *)

open Core.Std

let touch t s =
  let count =
    match List.Assoc.find t s with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count + 1)
```

The file `counter.ml` will be compiled into a module named `Counter`.
The name of the module is derived automatically from the filename.
Note that the module name is capitalized even if the file is not.

We can now rewrite `freq.ml` to use `Counter`.  Note that the
resulting code can still be built with `ocamlbuild`, which will
discover dependencies and realize that `counter.ml` needs to be
compiled.

```ocaml
(* freq.ml: using Counter *)
open Core.Std

let build_counts () =
  In_channel.fold_lines stdin ~init:[] ~f:Counter.touch

let () =
  build_counts ()
  |> List.sort counts ~cmp:(fun (_,x) (_,y) -> compare y x)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
```

## Signatures and Abstract Types

While we've pushed some of the logic to the `Counter` module, the code
in `freq.ml` can still depend on the details of the implementation of
`Counter`.  Indeed, if you look at the definition of `build_counts`:

```ocaml
let build_counts () =
  In_channel.fold_lines stdin ~init:[] ~f:Counter.touch
```

you'll see that it depends on the fact that the empty set of frequency
counts is represented as an empty list.  We'd like to prevent this
kind of dependency, so that we can change the implementation of
`Counter` without needing to change client code like that in
`freq.ml`.

The first step towards hiding the implementation details of `Counter`
is to write down the _signature_ of the module.  Note that the terms
_interface_, _signature_ and _module type_ are all used almost
interchangeably in the OCaml world.

In this case, we'll put the signature in `counter.mli`, which is the
interface file (that's where the `i` in `mli` comes from) for
`counter.ml`.  We'll start by writing down an interface that describes
what's currently available in `Counter` without hiding anything.
We'll use `val` declarations to specify values to expose.  The syntax
of a `val` declaration is as follows:

```
val <identifier> : <type>
```

And here's the contents of the `mli` file.

```ocaml
(* counter.mli: descriptive interface *)
open Core.Std

val touch : (string * int) list -> string -> (string * int) list
```

Note that `ocamlbuild` will detect the presence of the `mli` file
automatically and include it in the build.

<note><title>Auto-generating `mli` files</title>

In general, if you don't want to construct an mli entirely by hand,
you can ask OCaml to autogenerate one for you from the source, which
you can then adjust to fit your needs.  In this case, we can write:

```
$ ocamlbuild -use-ocamlfind counter.inferred.mli
```

Which will generate the file `_build/counter.inferred.mli`, with the
following contents.

```
$ cat _build/counter.inferred.mli
val touch :
  ('a, int) Core.Std.List.Assoc.t -> 'a -> ('a, int) Core.Std.List.Assoc.t
```

This is equivalent to the mli that we generated, but is a little more
verbose.  In general, you want to use autogenerated `mli`'s  as a
starting point only.  There's no replacement for a careful
consideration of what should be included in the interface of your
module and of how that should be organized, documented and formatted.

</note>


To actually hide the fact that frequency counts are represented as
association lists, we need to make the type of frequency counts
_abstract_.  A type is abstract if its name is exposed in the
interface, but its definition is not.  Here's an abstract interface
for `Counter`:

```ocaml
(* counter.mli: abstract interface *)

open Core.Std

type t

val empty : t
val to_list : t -> (string * int) list
val touch : t -> string -> t
```

Note that we needed to add `empty` and `to_list` to `Counter`, since
otherwise, there would be no way to create a `Counter.t` or get data
out of one.

Here's a rewrite of `counter.ml` to match this interface.

```ocaml
(* counter.ml: implementation matching abstract interface *)

open Core.Std

type t = (string * int) list

let empty = []

let to_list x = x

let touch t s =
  let count =
    match List.Assoc.find t s with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add t s (count + 1)
```

If we now try to compile `freq.ml`, we'll get the following error:

```
File "freq.ml", line 11, characters 20-22:
Error: This expression has type 'a list
       but an expression was expected of type Counter.t
```

This is because `freq.ml` depends on the fact that frequency counts
are represented as association lists, a fact that we've just hidden.
We just need to fix `build_counts` to use `Counter.empty` instead of
`[]` and `Counter.to_list` to get the association list out at the end
for processing and printing, as below.

```ocaml
let build_counts () =
  In_channel.fold_lines stdin ~init:Counter.empty ~f:Counter.touch
```

Now we can turn to optimizing the implementation of `Counter`.  Here's
an alternate and far more efficient implementation, based on the `Map`
datastructure in Core.

```ocaml
(* counter.ml: efficient version *)

open Core.Std

type t = int String.Map.t

let empty = String.Map.empty

let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.add t ~key:s ~data:(count + 1)
```

Note that in the above we use `String.Map` in some places and simply
`Map` in others.  This has to do with the fact that for some
operations, like creating a `Map.t`, you need access to
type-specialized information, and for others, like looking something
up in `Map.t`, you don't.  This is covered in more detail in
[xref](#maps-and-hashtables).

## Concrete types in signatures

In our frequency-count example, the module `Counter` had an abstract
type `Counter.t` for representing a collection of frequency counts.
Sometimes, you'll want to make a type in your interface _concrete_, by
including the type definition in the interface.

For example, imagine we wanted to add a function to `Counter` for
returning the line with the median frequency count.  If the number of
lines is even, then there is no precise median and the function would
return the lines before and after the median instead.  We'll use a
custom type to represent the fact that there are two possible return
values.  Here's a possible implementation.

```ocaml
type median = | Median of string
              | Before_and_after of string * string

let median t =
  let sorted_strings = List.sort (Map.to_alist t)
      ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  in
  let len = List.length sorted_strings in
  if len = 0 then failwith "median: empty frequency count";
  let nth n = fst (List.nth_exn sorted_strings n) in
  if len mod 2 = 1
  then Median (nth (len/2))
  else Before_and_after (nth (len/2 - 1), nth (len/2));;
```

Now, to expose this usefully in the interface, we need to expose both
the function and the type `median` with its definition.  Note that
values (of which functions are an example) and types have distinct
namespaces, so there's no name clash here.  The following two lines
added to `freq.mli` does the trick.

```ocaml
type median = | Median of string
              | Before_and_after of string * string

val median : t -> median
```

The decision of whether a given type should be abstract or concrete is
an important one.  Abstract types give you more control over how
values are created and accessed, and make it easier to enforce
invariants beyond what is enforced by the type itself; concrete types
let you expose more detail and structure to client code in a
lightweight way.  The right choice depends very much on the context.

## Nested modules

Up until now, we've only considered modules that correspond to files,
like `counter.ml`.  But modules (and module signatures) can be nested
inside other modules.  As a simple example, consider a program that
needs to deal with multiple identifier like usernames and hostnames.
If you just represent these as strings, then it becomes easy to
confuse one with the other.

A better approach is to mint new abstract types for each identifier,
where those types are under the covers just implemented as strings.
That way, the type system will prevent you from confusing a username
with a hostname, and if you do need to convert, you can do so using
explicit conversions to and from the string type.

Here's how you might create such an abstract type, within a
sub-module:

```ocaml
open Core.Std

module Username : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let of_string x = x
  let to_string x = x
end
```

Note that the `to_string` and `of_string` functions above are
implemented simply as the identity function, which means they have no
runtime effect.  They are there purely as part of the discipline that
they enforce on the code through the type system.

The basic structure of a module declaration like this is:

```ocaml
module <name> : <signature> = <implementation>
```

We could have written this slightly differently, by giving the
signature its own toplevel `module type` declaration, making it
possible to create multiple distinct types with the same underlying
implementation in a lightweight way.

```ocaml
open Core.Std

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module String_id = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = { user: Username.t;
                      host: Hostname.t;
                      when_started: Time.t;
                    }

let sessions_have_same_user s1 s2 =
  s1.user = s2.host
```

The above code has a fairly obvious bug, and indeed, the compiler will
refuse to compile it, spitting out the following error.

```
File "buggy.ml", line 25, characters 12-19:
Error: This expression has type Hostname.t
       but an expression was expected of type Username.t
Command exited with code 2.
```

We can also combine this with the use of the include statement to add
some extra functionality to such a module.  Thus, we could have
rewritten the definition of `Hostname` above as follows to add a
function `Hostname.mine` that returns the hostname of the present
machine.

```ocaml
module Hostname : sig
  include ID
  val mine : unit -> t
end = struct
  include String_id
  let mine = Unix.gethostname
end
```

## Opening modules

One useful primitive in OCaml's module language is the `open`
statement.  We've seen that already in the `open Core.Std` that has
been at the top of our source files.

We've used OCaml's `open` statement many times already in the `open

So far, we've been referring to values and types within a module by
using the module name as an explicit qualifier.  _e.g._, we write
`List.map` to refer to the `map` function in the `List` module
Sometimes, though, you want to be able to refer to the contents of a
module without that kind of explicit qualification.  This is what the
`open` statement is for.

We've already seen the `open` statement in use in the `open Core.Std`
statements at the top of each source file.  Opening a module adds its
contents to the environment that the compiler looks in for finding
identifiers.  Here's a trivial example that gives you a sense of how
this works.

```ocaml
# module M = struct let foo = 3 end;;
module M : sig val foo : int end
# foo;;
Error: Unbound value foo
# open M;;
# foo;;
- : int = 3
```

`open` is essential when you want to modify your environment for a
standard library like Core, but it's generally good style to keep
opening of modules to a minimum.  Opening a module is basically a
tradeoff between terseness and explicitness - the more modules you
open, the harder it is to look at an identifier and figure out where
it's defined.

Here's some general advice on how to deal with opens.

  * Opening modules at the toplevel of a module should be done quite
    sparingly, and generally only with modules that have been
    specifically designed to be opened, like `Core.Std` or
    `Option.Monad_infix`.

  * If you do need to do an open, it's better to do a _local open_.
    There are two syntaxes for local opens.  For example, you can
    write:

    ```ocaml
    let average x y =
      let open Int64 in
      x + y / of_int 2
    ```

    In the above, `of_int` and the infix operators are the ones from
    `Int64` module.

    There's another even more lightweight syntax for local opens, which
    is particularly useful for small expressions:

    ```ocaml
    let average x y =
      Int64.(x + y / of_int 2)
    ```

  * An alternative to local opens that makes your code terser without
    giving up on explicitness is to locally rebind the name of a
    module.  So, instead of writing:

    ```ocaml
    let print_median m =
       match m with
       | Counter.Median string -> printf "True median:\n   %s\n"
       | Counter.Before_and_after of before * after ->
         printf "Before and after median:\n   %s\n   %s\n" before after
    ```

    ...you could write

    ```ocaml
    let print_median m =
       let module C = Counter in
       match m with
       | C.Median string -> printf "True median:\n   %s\n"
       | C.Before_and_after of before * after ->
         printf "Before and after median:\n   %s\n   %s\n" before after
    ```

    Because the module name `C` only exists for a short scope, it's
    easy to read and remember what `C` stands for.  Rebinding modules
    to very short names at the toplevel of your module is usually a
    mistake.

## Including modules

While opening a module affects the environment used to search for
identifiers, _including_ a module is a way of actually adding new
identifiers to a module proper.  Consider the following simple module
for representing a range of intervals.

```ocaml
# module Interval = struct
    type t = | Interval of int * int
             | Empty

    let create low high =
      if high < low then Empty else Interval (low,high)
  end;;
module Interval :
  sig type t = Interval of int * int | Empty val create : int -> int -> t end
```

We can use the `include` directive to create a new, extended version
of the `Interval` module.

```ocaml
# module Extended_interval = struct
    include Interval

    let contains t x =
      match t with
      | Empty -> false
      | Interval (low,high) -> x >= low && x <= high
  end;;
module Extended_interval :
  sig
    type t = Interval.t = Interval of int * int | Empty
    val create : int -> int -> t
    val contains : t -> int -> bool
  end
# Extended_interval.contains (Extended_interval.create 3 10) 4;;
- : bool = true
```

The difference between `include` and `open` is that we've done more
than change how identifiers are searched for: we've changed what's in
the module.  If we'd used `open`, we'd have gotten a quite different
result.

```ocaml
# module Extended_interval = struct
    open Interval

    let contains t x =
      match t with
      | Empty -> false
      | Interval (low,high) -> x >= low && x <= high
  end;;
module Extended_interval :
  sig val contains : Extended_interval.t -> int -> bool end
# Extended_interval.contains (Extended_interval.create 3 10) 4;;
Error: Unbound value Extended_interval.create
```

To consider a more realistic example, imagine you wanted to build an
extended version of the `List` module, where you've added some
functionality not present in the module as distributed in Core.
`include` allows us to do just that.

```ocaml
(* ext_list.ml: an extended list module *)

open Core.Std

(* The new function we're going to add *)
let rec intersperse list el =
  match list with
  | [] | [ _ ]   -> list
  | x :: y :: tl -> x :: el :: intersperse (y::tl) el

(* The remainder of the list module *)
include List
```

Now, what about the interface of this new module?  It turns out that
include works on the signature language as well, so we can pull
essentially the same trick to write an `mli` for this new module.  The
only trick is that we need to get our hands on the signature for the
list module, which can be done using `module type of`.

```ocaml
(* ext_list.mli: an extended list module *)

open Core.Std

(* Include the interface of the list module from Core *)
include (module type of List)

(* Signature of function we're adding *)
val intersperse : 'a list -> 'a -> 'a list
```

Note that the order of declarations in the `mli` does not need to
match the order of declarations in the `ml`.  Also, the order of
declarations in the `ml` is quite important in that it determines what
values are shadowed.  If we wanted to replace a function in `List`
with a new function of the same name, the declaration of that function
in the `ml` would have to come after the `include List` declaration.

And we can now use `Ext_list` as a replacement for `List`.  If we want
to use `Ext_list` in preference to `List` in our project, we can
create a file of common definitions:

```ocaml
(* common.ml *)

module List = Ext_list
```

And if we then put `open Common` after `open Core.Std` at the top of
each file in our project, then references to `List` will automatically
go to `Ext_list` instead.

## Common errors with modules

When OCaml compiles a program with an `ml` and an `mli`, it will
complain if it detects a mismatch between the two.  Here are some of
the common errors you'll run into.

### Type mismatches

The simplest kind of error is where the type specified in the
signature does not match up with the type in the implementation of the
module.  As an example, if we replace the `val` declaration in
`counter.mli` by swapping the types of the first two arguments:

```ocaml
val touch : string -> t -> t
```

and then try to compile `Counter` (by writing `ocamlbuild
-use-ocamlfind counter.cmo`.  The `cmo` file is a compiled object
file, containing the bytecode-compiled version of a module), we'll get
the following error:

```
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       Values do not match:
         val touch :
           ('a, int) Core.Std.Map.t -> 'a -> ('a, int) Core.Std.Map.t
       is not included in
         val touch : string -> t -> t
```

This error message is a bit intimidating at first, and it takes a bit
of thought to see why the first type for touch (which comes from the
implementation) doesn't match the second one (which comes from the
interface).  The key thing to remember is that `t` is a
`Core.Std.Map.t`, at which point you can see that the error is a
mismatch in the order of arguments to `touch`.

There's no denying that learning to decode such error messages is
difficult at first, and takes some getting used to.  But in time,
decoding these errors becomes second nature.

### Missing definitions

We might decide that we want a new function in `Counter` for pulling
out the frequency count of a given string.  We can update the `mli` by
adding the following line.

```ocaml
val count : t -> string -> int
```

Now, if we try to compile without actually adding the implementation,
we'll get this error:

```
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       The field `count' is required but not provided
```

A missing type definition will lead to a similar error.

### Type definition mismatches

Type definitions that show up in an `mli` need to match up with
corresponding definitions in the `ml`.  Consider again the example of
the type `median`.  The order of the declaration of variants matters
to the OCaml compiler so, if the definition of `median` in the
implementation lists those options in a different order:

```ocaml
type median = | Before_and_after of line * line
              | Median of line
```

that will lead to a compilation error:

```
File "counter.ml", line 1, characters 0-1:
Error: The implementation counter.ml
       does not match the interface counter.cmi:
       Type declarations do not match:
         type median = Before_and_after of string * string | Median of string
       is not included in
         type median = Median of string | Before_and_after of string * string
       Their first fields have different names, Before_and_after and Median.
```

Order is similarly important in other parts of the signature,
including the order in which record fields are declared and the order
of arguments (including labeled and optional arguments) to a
function.

### Cyclic dependencies

In most cases, OCaml doesn't allow circular dependencies, _i.e._, a
collection of definitions that all refer to each other.  If you want
to create such definitions, you typically have to mark them specially.
For example, when defining a set of mutually recursive values (like
the definition of `is_even` and `is_odd` in
[xref](#recursive-functions)), you need to define them using `let rec`
rather than ordinary `let`.

The same is true at the module level.  By default, circular
dependencies between modules are not allowed, and indeed, circular
dependencies among files are never allowed.

The simplest case of this is that a module can not directly refer to
itself (although definitions within a module can refer to each other
in the ordinary way).  So, if we tried to add a reference to `Counter`
from within `counter.ml`:

```ocaml
let singleton l = Counter.touch Counter.empty
```

then when we try to build, we'll get this error:

```
File "counter.ml", line 17, characters 18-31:
Error: Unbound module Counter
Command exited with code 2.
```

The problem manifests in a different way if we create circular
references between files.  We could create such a situation by adding
a reference to Freq from `counter.ml`, _e.g._, by adding the following
line:

```ocaml
let build_counts = Freq.build_counts
```

In this case, `ocamlbuild` will notice the error and complain:

```
Circular dependencies: "freq.cmo" already seen in
  [ "counter.cmo"; "freq.cmo" ]
```

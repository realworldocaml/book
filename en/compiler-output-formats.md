# The Compilation Pipeline

Compiling source code into executable programs is a fairly complex process that
involves quite a few tools -- preprocessors, compilers, runtime libraries,
linkers and assemblers.  It's important how to understand how these fit
together to help with your day-to-day workflow of developing, debugging,
maintaining and deploying applications.

OCaml has a strong emphasis on static type safety and tries to reject source
code that doesn't meet its requirements as early as possible.  The compiler
toolchain implements these checks as a series of transformations that start
with the source code. Each stage checks some properties and discards
information from the previous stage, until the final output is low-level
assembly code that no longer knows anything about OCaml modules or objects.

TODO note about how this different from JVM/NET w type erasure?

Much of this complexity is hidden away via the OCaml build system.  However,
you'll sometime need to dive into the toolchain to hunt down a bug or
investigate a performance regression.  In these cases, you can manually run a
stage and examine the output more closely.

It's even possible to compile OCaml to run efficiently on environments such as
Javascript or the Java Virtual Machine.  These are third-party tools that
extend the core compilation toolchain, and so it helps to understand how
everything fits together before starting to use them.

In this chapter, you'll learn:

* the compilation pipeline and what each stage represents.
* source preprocessing via `camlp4` and the intermediate forms.
* the bytecode `ocamlc` compiler and `ocamlrun` interpreter.
* the native code `ocamlopt` code generator.

## An overview of the toolchain

The OCaml tools accept textual source code as input with filename extensions of
`.ml` and `.mli` for modules and signatures respectively.  Each source file
represents a *compilation unit* that is built separately.  The compiler
generates intermediate files with different filename extensions to use as it
advances through the compilation stages.  Finally, the compiler linker gathers
together a collection of compiled compilation units and produces a standalone
executable or library archive that can be re-used by other applications.

The overall compilation pipeline looks like this:

```
    Source code
        |
        | parsing and preprocessing
        v
    Parsetree (untyped AST)
        |
        | syntax extensions
        v
    Camlp4 transformation (untyped AST)
        |
        | type inference and checking
        v
    Typedtree (type-annotated AST)
        |
        | pattern-matching compilation
        | elimination of modules and classes
        v
     Lambda
      /  \
     /    \ closure conversion, inlining, uncurrying,
    v      \  data representation strategy
 Bytecode   \
    |        +-----+
    |             Cmm
    |js_of_ocaml   |
    |              | code generation
    |              v
 Javascript     Assembly code
```

Notice that the pipeline branches towards the end. This is because OCaml has
multiple compiler frontends that re-use the early stages of compilation, but
produce very different final outputs.  The bytecode interpreter is portable and
lightweight (and can be transformed into Javascript), whereas the native code
compiler generates specialized native code binaries suitable for
high-performance applications.

### Obtaining the source code

Although it's not necessary to understand the examples, you may find it useful
to have a copy of the OCaml source tree checked out while you read through this
chapter.  The source code is available from multiple places:

* Stable releases as zip and tar archives from the [OCaml download site](http://caml.inria.fr/download.en.html).
* A Subversion anonymous mirror of the main development sources available on the [development resources](http://caml.inria.fr/ocaml/anonsvn.en.html) page online.
* A Git mirror of the Subversion repository with all the history and development branches included, browsable online at [Github](https://github.com/ocaml/ocaml).

Once you've checked out a copy of the source tree, it has a few sub-directories
(TODO: describe depending on the contents below).
 
We'll now go go through the compilation stages and explain how the tools behind
them will be useful to you during OCaml development.

## Parsing source code

The first thing the compiler does is parse the input source files into a more
structured data type.  The compiler lexes the source text into a stream of
tokens and parses them into an Abstract Syntax Tree (AST) data structure.  This
parser is implemented in OCaml itself, using the techniques described earlier
in [xref](#parsing-with-ocamllex-and-menhir).  The lexer and parser rules can
be found in the `ocaml/parsing/` directory in the source distribution.

Here's a syntax error that we obtain by using the `module` keyword
incorrectly inside a `let` binding.

```ocaml
(* broken_module.ml *)
let _ =
  module MyString = String;
  ()
```

The fixed version of this locally opens the `module` correctly.

```ocaml
(* fixed_module.ml *)
let _ =
  let module MyString = String in
  ()
```

Parsing immediately eliminates code which doesn't match basic syntactic
requirements, so the first example will result in a syntax error when we run
the OCaml compiler on it. 

```console
$ ocamlc -c broken_module.ml 
File "broken_module.ml", line 3, characters 2-8:
Error: Syntax error
$ ocamlc -c fixed_module.ml 
```

The syntax error points to the line and character number of the first token
that couldn't be parsed.  In the broken example, the `module` keyword
shouldn't be encountered at that point in the `let` binding, so the error
location information is correct.

### Formatting source code with `ocp-indent`

Sadly, syntax errors do get more inaccurate sometimes, particularly in large
codebases. Try to spot the deliberate error in the following function
definitions.

```ocaml
(* follow_on_function.ml *)
let concat_and_print x y =
  let v = x ^ y in
  print_endline v;
  v;

let add_and_print x y =
  let v = x + y in
  print_endline (string_of_int v);
  v

let _ =
  let _ = add_and_print 1 2 in
  let _ = concat_and_print "a" "b" in
  ()
```

When you compile this file, you'll get a syntax error.

```console
$ ocamlc -c follow_on_function.ml
File "follow_on_function.ml", line 12, characters 0-3:
Error: Syntax error
```

The line number in the error points to the end of the `add_and_print` function,
but the actual error is at the end of the *first* function definition. There's
an extra semicolon at the end of the first definition that causes the second
definition to become part of the first `let` binding.  This eventually results
in a syntax error in the second function.

This class of bug (due to a single errant character) can be hard to spot in a
large body of code. Luckily, there's a great tool available in OPAM called
`ocp-indent` that applies structured indenting rules to your source code. This
not only beautifies your code layout, but it also works with partially
written code and makes this syntax error much more obvious.

Let's run our erronous file through `ocp-indent` and see how it processes it.

```console
$ opam install ocp-indent
$ ocp-indent follow_on_function.ml
(* follow_on_function.ml *)
let concat_and_print x y =
  let v = x ^ y in
  print_endline v;
  v;

  let add_and_print x y =
    let v = x + y in
    print_endline (string_of_int v);
    v

let _ =
  let _ = add_and_print 1 2 in
  let _ = concat_and_print "a" "b" in
  ()
```

The `add_and_print` definition has been indented as if it were part of the
first `concat_and_print` definition, and the errant semicolon is now much
easier to spot.  If we remove that semicolon, then the result is exactly what
we expect.

```console
$ ocp-indent follow_on_function_fixed.ml 
(* follow_on_function_fixed.ml *)
let concat_and_print x y =
  let v = x ^ y in
  print_endline v;
  v

let add_and_print x y =
  let v = x + y in
  print_endline (string_of_int v);
  v

let _ =
  let _ = add_and_print 1 2 in
  let _ = concat_and_print "a" "b" in
  ()
$ ocamlc -i follow_on_function_fixed.ml 
val concat_and_print : string -> string -> string
val add_and_print : int -> int -> int
```

The `ocp-indent` [homepage](https://github.com/OCamlPro/ocp-indent) has details
on how to integrate it with your favourite editor.  All the Core libraries are
formatted by `ocp-indent`, so it's a good idea to match this style when you
publish your own open-source project online.

### Generating documentation via `ocamldoc`

Whitespace and source code comments are removed during parsing and aren't
significant in determining the semantics of the program for compilation
purposes. However, other tools in the OCaml distribution can interpret them for
their own ends.

The `ocamldoc` tool looks for specially formatted comments in the source code
to generate documentation bundles. These comments are combined with the
function definitions and signatures, and output as structured documentation in
a variety of formats. `ocamldoc` support generating HTML pages, LaTeX and PDF
documents, UNIX manual pages, and even module dependency graphs that can be
viewed using [Graphviz](http://www.graphviz.org).

Here's a sample of some source code that's been annotated with `ocamldoc`-style
comments.

```ocaml
(** example.ml: The first special comment of the file is the comment 
    associated with the whole module. *)

(** Comment for exception My_exception. *)
exception My_exception of (int -> int) * int

(** Comment for type [weather]  *)
type weather =
| Rain of int (** The comment for construtor Rain *)
| Sun         (** The comment for constructor Sun *)

(** Find the current weather for a country
   @author Anil Madhavapeddy
   @param location The country to get the weather for.
*)
let what_is_the_weather_in location =
  match location with
  | `Cambridge  -> Rain 100
  | `New_york   -> Rain 20
  | `California -> Sun
```

The `ocamldoc` comments are distinguished by beginning with the double asterix,
with some text formatting conventions within the comment to mark metadata (most
notably, the `@tag` fields to mark specific properties such as the author or
argument description).

Try compiling the HTML documentation and UNIX man pages by running `ocamldoc`
over the source file.

```console
$ mkdir -p html man/man3
$ ocamldoc -html -d html example.ml
$ ocamldoc -man -d man/man3 example.ml
$ man -M man Example
```

You should now have HTML files inside the `html/` directory, and be able to
view the UNIX manual pages held in `man/man3`.  There are quite a few comment
formats and options to control the output, so refer to the [OCaml
manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html) for the
complete list.

<tip>
<title>Using custom ocamldoc generators</title>

The default HTML output stylesheets from `ocamldoc` are pretty spartan and
distinctly Web 1.0.  The tool supports plugging in custom documentation
generators, and there are several available that provide prettier or more
detailed output.

* [Argot](http://argot.x9c.fr/) is an enchanced HTML generator that supports
  code folding and searching by name or type definition.
* The custom generators available
  [here](https://gitorious.org/ocamldoc-generators/ocamldoc-generators) add
  support for Bibtex references within comments and generating literate
  documentation that embeds the code alongside the comments.
* JSON output is available via `odoc_json` (TODO: avsm, pull out of Xen).

</tip>

### Preprocessing with `camlp4`

One powerful feature in OCaml is the facility to extend the language grammar
via the `camlp4` macro tool.  Camlp4 modules can register new keywords with the
lexer and later transform these keywords (or indeed, any portion of the input
program) into conventional OCaml code that can be understood by the rest of the
compiler.

We've already seen several examples of using `camlp4` within Core:

* `Fieldslib` to generates first-class values that represent fields of a record.
* `Sexplib` to convert types to textual s-expressions.
* `Bin_prot` for efficient binary conversion and parsing.

These all extend the language with a single keyword that turns a `type` declaration
into a `type <..> with` construct.

```ocaml
(* type_conv_example.ml *)
open Sexplib.Std

type t = {
  foo: int;
  bar: string
} with sexp, fields
```

Compiling this code will normally give you a syntax error.

```console
$ ocamlfind ocamlc -c type_conv_example.ml
File "type_conv_example.ml", line 7, characters 2-6:
Error: Syntax error
```

However, if you add in the ocamlfind syntax extension packages for `fieldslib`
and `sexplib`, then all works again.

```console
$ ocamlfind ocamlc -c -syntax camlp4o -package sexplib.syntax \
  -package fieldslib.syntax type_conv_example.ml
```

The syntax extension packages cause `ocamlfind` to generate a `-pp` option to
the compiler, ith the preprocessing modules as arguments.  These preprocessor
modules are dynamically loaded into the `camlp4o` command-line tool, which
rewrites the input token stream from the source code into conventional OCaml
code that has no trace of the new keywords.  The compiler then continues on to
compile this transformed code.

Both `fieldslib` and `sexplib` need this `with` keyword, so there is a common
preprocessor library called `type_conv` that provides the extension framework
for them to use.
Type_conv defines the `with` grammar extension and `ocamlfind` ensures that it's
loaded before `variantslib` or `sexplib`.

The preprocessor extensions can now generate boiler-plate OCaml code based on
the type definition.  This approach avoids the inevitable performance hit of
doing this generation dynamically, but also doesn't require a complex
Just-In-Time (JIT) runtime that is a source of unpredictable dynamic behaviour.
Instead, all code is generated at compilation time via `camlp4`.

### Inspecting `camlp4` output

All `camlp4` modules accept an input AST and output a modified one.  If you're
not familiar with the `camlp4` module in question, then you need to figure out 
what transformations it has applied to your source code.  The first obvious way
is to read the documentation that accompanies the package, but you can also
use the top-level to explore this, or run `camlp4` manually.

#### Using `camlp4` in the interactive top-level

The `utop` top-level can run phrase that you type in through `camlp4`
automatically.  To get this to work, you should have at least these lines
in your `~/.ocamlinit` file in your home directory (the contents of this file
are automatically executed every time you run `utop`, so you can run them
by hand too).

```
#use "topfind"
#camlp4o
```

The first directive loads the extra `ocamlfind` top-level commands, which let
you load `ocamlfind` packages directly (including all their dependent packages).
The second directive instructs the top-level to filter all future phrases
via `camlp4`.

You can now run `utop` and load the syntax extension you want to experiment
with.  We'll show you the `comparelib` syntax extension from Core.  OCaml
provides a polymorphic comparison operator that inspects the runtime
representation of two values to see if they are equal.  As we noted in
[xref](#maps-and-hashtables), this is not as efficient or as safe as defining
explicit comparison functions between values.

Let's see how `comparelib` solves this problem in `utop`.

```ocaml
# #require "comparelib.syntax" ;;

# type t = { foo: string; bar : t } ;;
type t = { foo : string; bar : t; }

# type t = { foo: string; bar: t } with compare ;;
type t = { foo : string; bar : t; }
val compare : t -> t -> int = <fun>
val compare_t : t -> t -> int = <fun>
```

The first type definition of `t` is a standard OCaml phrase and results in the
expected output.  The second one includes the `with compare` directive.  This
is intercepted by `comparelib` and turned into two new functions that are generated
from the type into the `compare` and `compare_t` functions.

#### Running `camlp4` directly on the source code

While the top-level is quick and useful to figure out the function signatures
generated from the extensions, how can we see what these functions actually do?
You can't do this from `utop` directly, since it embeds the `camlp4`
compilation as an automated part of its operation.

Let's turn to the command-line to inspect the result of the `comparelib`
transformation instead.  Create a file that contains the type declaration from
earlier:

```ocaml
(* comparelib_test.ml *)
type t = { 
  foo: string; 
  bar: t
} with compare
```

Now create a shell script that runs `camlp4` directly.

```bash
#!/bin/sh
# camlp4_dump

OCAMLFIND="ocamlfind query -predicates syntax,preprocessor -r"
INCLUDE=`$OCAMLFIND -i-format comparelib.syntax`
ARCHIVES=`$OCAMLFIND -a-format comparelib.syntax`
camlp4o -printer o $INCLUDE $ARCHIVES $1
```

This shell script uses the `ocamlfind` package manager to list the include and
library paths needed by `comparelib`.  The final command invokes the `camlp4o`
preprocessor directly and outputs the resulting AST to standard output as
textual source code.

```console
$ sh camlp4_dump comparelib_test.ml
type t = { foo : string; bar : t }

let _ = fun (_ : t) -> ()
  
let rec compare : t -> t -> int =
  fun a__001_ b__002_ ->
    if Pervasives.( == ) a__001_ b__002_
    then 0
    else
      (let ret =
         (Pervasives.compare : string -> string -> int) a__001_.foo
           b__002_.foo
       in
         if Pervasives.( <> ) ret 0
         then ret
         else compare a__001_.bar b__002_.bar)
  
let _ = compare
let compare_t = compare
let _ = compare_t
```

The output code contains the original type definition accompanied by some
automatically generated code that implements an explicit comparison function
for each field in the record.  If you're using the extension in your compiler
command-line, this generated code is then compiled as if you had typed it in
yourself.

Another useful feature of `type_conv` is that it can generate module signatures
too.  Copy the earlier type definition into a `comparelib_test.mli` and rerun
the camlp4 dumper script.

```console
$ ./camlp4_dump.sh test_comparelib.mli 
type t = { foo : string; bar : t }

val compare : t -> t -> int
```

The external signature generated by `comparelib` is much simpler than the
actual code.  Running `camlp4` directly on the original source code lets you
see these all these transformations precisely.

<note>
<title>Don't overdo the syntax extensions</title>

Syntax extensions are a very powerful extension mechanism that can completely
change your source code's layout and style.  Core includes a very conservative
set of extensions that minimise the syntax changes.  There are a number of
third-party libraries that perform much more wide-sweeping changes, such as
introducing whitespace-sensitive indentation or even building entirely new
languages.

While it's tempting to compress all your boiler-plate code into `camlp4`
extensions, it can make production source code much harder for other people to
read and review.  Core mainly focuses on type-driven code generation using the
`type_conv` extension, and doesn't fundamentally change the OCaml syntax.

Another thing to consider before deploying your own syntax extension is
compatibility with other syntax extensions.  Two separate extensions create a
grammar clash can lead to hard-to-reproduce bugs. That's why most of Core's
syntax extensions go through `type_conv`, which acts as a single point for
extending the grammar via the `with` keyword.

</note>

We've only shown you how to use `camlp4` extensions here. The full details of
building new extensions with `camlp4` are fairly daunting and could be the
subject of an entirely new book.  The best resources to start with this are the
online [Camlp4 wiki](http://brion.inria.fr/gallium/index.php/Camlp4) and also
by searching for existing extensions using OPAM and seeing how they implement
their particular grammar extensions.  A series of [blog posts](http://ambassadortothecomputers.blogspot.co.uk/p/reading-camlp4.html)
are also a useful guide to the internals of extensions.

## The type checking phase

After obtaining a valid parsed AST (with or without `camlp4`) the compiler must
then check that the code obeys the rules of the static type system.  Code that
is syntactically correct but misuses values is rejected with an explanation of
the problem at this stage.  We're not going to delve into the details of how
type-checking works here (the rest of the book covers that), but rather how it
fits in with the rest of the compilation process.

Assuming that the source code is validly typed, the original AST is transformed
into a *typed AST*. This has the same broad structure of the untyped AST, but
syntactic phrases are replaced with typed variants instead.

You can explore the type checking process very simply.  Create a file with a
single type definition and value.

```ocaml
(* typedef.ml *)
type t = Foo | Bar
let v = Foo
```

Now run the compiler on this file to *infer* a default type for the compilation
unit.  This will run the type checking process on the compilation unit you
specify.

```console
$ ocamlc -i typedef.ml
type t = Foo | Bar
val v : t
```

The type checker has run and provided a default signature for the module.  It's
often useful to redirect this output to an `mli` file to give you a starting
signature to edit the external interface, without having to type it all in by
hand.

### Using `ocamlobjinfo` to inspect compilation units

Recall from [xref](#files-modules-and-programs) that `mli` files are optional.
If you don't supply an explicit signature, then the inferred output from the
module implementation is used instead.  The compiled version of the signature
is stored in a filename ending with `cmi`.

```console
$ ocamlc -c typedef.ml
$ ocamlobjinfo typedef.cmi
File typedef.cmi
Unit name: Typedef
Interfaces imported:
	559f8708a08ddf66822f08be4e9c3372	Typedef
	65014ccc4d9329a2666360e6af2d7352	Pervasives
```

The `ocamlobjinfo` command examines the compiled interface and displays what
other compilation units it depends on.  In this case, we don't use any external
modules other than `Pervasives`.  Every module depends on `Pervasives` by
default, unless you use the `-nopervasives` flag (this is an advanced use-case,
and you shouldn't need it in normal use).

The long alphanumeric identifier beside each module name is a hash calculated
from all the types and values exported from that compilation unit.  This is
used during type-checking and linking to check that all of the compilation
units have been compiled consistently against each other.  A difference in the
hashes means that a compilation unit with the same module name may have
conflicting type signatures in different modules.  This violates static type
safety if these conflicting instances are ever linked together, as every
well-typed value has precisely one type in OCaml.

This hash check ensures that separate compilation remains type safe all the way
up to the final link phase.  Each source file is type checked separately, but
the hash of the signature of any external modules is recorded with the compiled
signature.

### Examining the typed syntax tree

The compiler has a couple of advanced flags that can dump the raw output of the
internal AST representation.  You can't depend on these flags to give the same
output across compiler revisions, but they are a useful learning tool.

First, let's look at the untyped AST from our `typedef.ml`.

```console
$ ocamlc -dparsetree typedef.ml
[
  structure_item (typedef.ml[1,0+0]..[1,0+18])
    Pstr_type [
      "t" (typedef.ml[1,0+5]..[1,0+6])
        type_declaration (typedef.ml[1,0+5]..[1,0+18])
          ptype_params = []
          ptype_cstrs = []
          ptype_kind =
            Ptype_variant
              [
                (typedef.ml[1,0+9]..[1,0+12])
                  "Foo" (typedef.ml[1,0+9]..[1,0+12])
                  [] None
                (typedef.ml[1,0+15]..[1,0+18])
                  "Bar" (typedef.ml[1,0+15]..[1,0+18])
                  [] None
              ]
          ptype_private = Public
          ptype_manifest = None
    ]
  structure_item (typedef.ml[2,19+0]..[2,19+11])
    Pstr_value Nonrec [
      <def>
        pattern (typedef.ml[2,19+4]..[2,19+5])
          Ppat_var "v" (typedef.ml[2,19+4]..[2,19+5])
        expression (typedef.ml[2,19+8]..[2,19+11])
          Pexp_construct "Foo" (typedef.ml[2,19+8]..[2,19+11])
          None false
    ]
]
```

This is rather a lot of output for a simple two-line program, but also reveals
a lot about how the compiler works.  Each portion of the tree is decorated with
the precise location information (including the filename and character location
of the token).  This code hasn't been type checked yet, and so the raw tokens
are all included.  After type checking, the structure is much simpler.

```console
$ ocamlc -dtypedtree typedef.m
[
  structure_item (typedef.ml[1,0+0]..typedef.ml[1,0+18])
    Pstr_type [
      t/1008
        type_declaration (typedef.ml[1,0+5]..typedef.ml[1,0+18])
          ptype_params = []
          ptype_cstrs = []
          ptype_kind =
            Ptype_variant
              [
                "Foo/1009" []
                "Bar/1010" []
              ]
          ptype_private = Public
          ptype_manifest = None
    ]
  structure_item (typedef.ml[2,19+0]..typedef.ml[2,19+11])
    Pstr_value Nonrec [
      <def>
        pattern (typedef.ml[2,19+4]..typedef.ml[2,19+5])
          Ppat_var "v/1011"
        expression (typedef.ml[2,19+8]..typedef.ml[2,19+11])
          Pexp_construct "Foo" [] false
    ]
]
```

The typed AST is more explicit than the untyped syntax tree.  For instance, the
type declaration has been given a unique name (`t/1008`), as has the `v` value
(`v/1011`).

You'll never need to use this information in day-to-day development, but it's
always instructive to examine how the type checker folds in the source code
into a more compact form like this.

### The untyped lambda form

Once OCaml gets past the typed AST, it eliminates all the static type
information into a simpler intermediate *lambda form*.  This discards all the
modules and objects, replacing them direct references to values such as records
and function pointers instead.  Pattern matches are compiled into automata that
are highly optimized for the particular type involved.

It's possible to examine all this behaviour via another intermediate output
from the compiler.  Create a new `pattern.ml` file alongside the previous
`typedef.ml`.

```ocaml
(* pattern.ml *)
open Typedef
let _ =
  match v with
  | Foo -> "foo"
  | Bar -> "bar"
```

The lambda form is the first representation that discards the OCaml type
information and begins to look like the runtime memory model from
[xref](#memory-representation-of-values), and should be quite familiar to Lisp
aficionados.  To see it for `pattern.ml`, compile as usual but add the
`-dlambda` directive.

```console
$ ocamlc -dlambda -c pattern.ml 
(setglobal Pattern!
  (seq
    (let (match/1008 (field 0 (global Typedef!)))
      (if (!= match/1008 0) "bar" "foo"))
    (makeblock 0)))
```

It's not important to understand every detail of this internal form, but
some interesting points are:

* There are no mention of modules or types any more, and have been replaced by
  references to global values instead.
* The pattern match has turned into an integer comparison by checking 
  the header tag of `v`.  Recall that variants without parameters are stored 
  in memory as integers in the order which they appear.  The pattern matching 
  engine knows this, and has transformed the pattern into a single integer
  comparison.  If `v` has a tag of `0`, the function returns `"foo"`, and otherwise
  returns `"bar"`.
* Values are addressed directly by an index and `field` (`v` got assigned to `1008`
  during type checking).  The type safety checks earlier have ensured that these
  fields will always exist, and so the lambda form doesn't do any dynamic checks.
  However, unwise use of unsafe language features such as `Obj.magic` module can
  easily induce crashes at this level.

The lambda form is primarily a stepping-stone to the bytecode engine that we
cover next.  However, it's often easier to look at the textual output here than
wade through native assembly code from compiled executables.

## Portable bytecode with `ocamlc` and `ocamlrun`

After the lambda form has been generated, we are very close to having
executable code.  The OCaml tool-chain branches into two separate compilers at
this point.  We'll describe the the `ocamlc` bytecode compiler first, which consists
of two pieces:

* `ocamlc` compiles files into a simple bytecode that is a close mapping to the lambda form.
* `ocamlrun` is a portable interpreter that executes the bytecode.

`ocamlrun` is an interpreter that uses the OCaml stack and an accumulator to
store values, and only has seven registers in total (the program counter, stack
pointer, accumulator, exception and argument pointers, and environment and
global data).  It implements around 140 opcodes which form the OCaml program.
The full details of the opcodes are available
[online](http://cadmium.x9c.fr/distrib/caml-instructions.pdf).

The big advantage of using `ocamlc` is simplicity, portability and compilation
speed.  The mapping from the lambda form to bytecode is straightforward, and
this results in predictable (but slow) execution speed.

### Compiling and linking OCaml bytecode

`ocamlc` compiles individual `ml` files into bytecode `cmo` files.  These are
linked together with the OCaml standard library to produce an executable
program.  The order in which `.cmo` arguments are presented on the command line
defines the order in which compilation units are initialized at runtime (recall
that OCaml has no single `main` function like C does). 

A typical OCaml library consists of multiple modules (and hence multiple `cmo`
files).  `ocamlc` can combine these into a single `cma` bytecode archive by
using the `-a` flag. The objects in the library are linked as regular `cmo`
files in the order specified when the library file was built.  However, if an
object file within the library isn't referenced elsewhere in the program, it is
not linked unless the `-linkall` flag forces it to be included.  This behaviour
is analogous to how C handles object files and archives (`.o` and `.a`
respectively).

The bytecode runtime comprises three main parts: the bytecode interpreter,
garbage collector, and a set of C functions that implement the primitive
operations.  Bytecode instructions are provided to call these C functions.  The
OCaml linker produces bytecode for the standard runtime system by default, and
any custom C functions in your code (e.g. from C bindings) require the runtime
to dynamically load a shared library.

This can be specified as follows:

```console
$ ocamlc -a -o mylib.cma a.cmo b.cmo -dllib -lmylib
```

The `dllib` flag embeds the arguments in the archive file.  Any subsequent
packages including this archive will also include the extra linking directive.
This lets the `ocamlrun` runtime locate the extra symbols when it executes the
bytecode.

You can also generate a complete standalone executable that bundles the `ocamlrun`
interpreter with the bytecode in a single binary.  This is known as the "custom"
runtime mode, and can be run by:

```console
$ ocamlc -a -o mylib.cma -custom a.cmo b.cmo -cclib -lmylib
```

The custom mode is the most similar to native code compilation, as both
generate standalone executables.  There are quite a few other options available
for compiling bytecode (notably with shared libraries or building custom
runtimes).  Full details can be found in the
[manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual022.html).

#### Embedding OCaml bytecode

A consequence of using the bytecode compiler is that the final link phase must
be performed by `ocamlc`.  However, you might sometimes want to embed your OCaml
code inside an existing C application.  OCaml also supports this mode of operation
via the `-output-obj` directive.

This mode causes `ocamlc` to output a C object file that containing the
bytecode for the OCaml part of the program, as well as a `caml_startup`
function.  The object file can then be linked with other C code using the
standard C compiler, or even turned in a standalone C shared library.

The bytecode runtime library is installed as `libcamlrun` in the standard OCaml
directory (obtained by `ocamlc -where`).  Creating an executable just requires
you to link the runtime library with the bytecode object file.  Here's a quick
example to show how it all fits together.

Create two OCaml source files that contain a single print line.

```console
$ cat embed_me1.ml 
let () = print_endline "hello embedded world 1"
$ cat embed_me2.ml 
let () = print_endline "hello embedded world 2"
```

Next, create a C file which will be your main entry point.

```c
/* main.c */
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

int 
main (int argc, char **argv)
{
  puts("Before calling OCaml");
  caml_startup (argv);
  puts("After calling OCaml");
  return 0;
}
```

Now compile the OCaml files into a standalone object file.

```console
$ ocamlc -output-obj -o embed_out.o embed_me1.ml embed_me2.ml
```

After this point, you no longer need the OCaml compiler, as `embed_out.o` has
all of the OCaml code compiled and linked into a single object file.  Compile
an output binary using gcc to test this out.

```console
$ gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lcamlrun -ltermcap \
  -o final_out embed_out.o main.c
$ ./final_out 
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

Once inconvenience with `gcc` is that you need to specify the location
of the OCaml library directory.  The OCaml compiler can actually handle C
object and source files, and it adds the `-I` and `-L` directives for you.  You
can compile the previous object file with `ocamlc` to try this.

```console
$ ocamlc -o final_out2 embed_out.o main.c
$ ./final_out2
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

You can also verify the system commands that `ocamlc` is invoking by adding
`-verbose` to the command line.  You can even obtain the source code to
the `-output-obj` result by specifying a `.c` output file extension instead
of the `.o` we used earlier.

```console
$ ocamlc -output-obj -o embed_out.c embed_me1.ml embed_me2.ml
$ cat embed_out.c
```

Embedding OCaml code like this lets you write OCaml code that interfaces with
any environment that works with a C compiler.   You can even cross back from the
C code into OCaml by using the `Callback` module to register named entry points
in the OCaml code.  This is explained in detail in the
[interfacing with C](http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html#toc149)
section of the OCaml manual.

## Native code generation

The native code compiler is ultimately the tool that production OCaml code goes
through.  It compiles the lambda form into fast native code executables, with
cross-module inlining and code optimization passes that the bytecode
interpreter doesn't perform.  However, care is taken to ensure compatibility
with the bytecode runtime, and the same code should run identically when
compiled with either toolchain.

The `ocamlopt` command is the frontend to the native code compiler, and has a
very similar interface to `ocamlc`.  It also accepts `ml` and `mli` files, but
compiles them to:

* A `.o` file containing native object code.
* A `.cmx` file containing extra information for linking and cross-module optimization.
* A `.cmi` compiled interface file that is the same as the bytecode compiler.

When the compiler links modules together into an executable, it uses the
contents of the `cmx` files to perform cross-module inlining across compilation
units.  This can be a significant speedup for standard library functions that
are frequently used outside of their module.

Collections of `.cmx` and `.o` files can also be be linked into a `.cmxa`
archive by passing the `-a` flag to the compiler.  However, unlike the bytecode
version, you must keep the individual `cmx` files in the compiler search path
so that they are available for cross-module inlining.  If you don't do this,
the compilation will still succeed, but you will have missed out on an
important optimization and have slower binaries.

### Building debuggable libraries

The native code compiler builds executables that can be debugged using
conventional system debuggers such as GNU `gdb`.  You'll need to compile your
libraries with the `-g` option to add the debug information to the output, just
as you need to with C compilers.

TODO add example of gdb breakpoint use

#### Profiling native code libraries

TODOs

#### Embedding native code in libraries

The native code compiler also supports `output-obj` operation just like the
bytecode compiler.  The native code runtime is called `libasmrun.a`, and should
be linked instead of the bytecode `libcamlrun.a`.

Try this out using the same files from the bytecode embedding example earlier.

```console
$ ocamlopt -output-obj -o embed_native.o embed_me1.ml embed_me2.ml
$ gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lasmrun -ltermcap \
  -o final_out_native embed_native.o main.c
./final_out_native
Before calling OCaml
hello embedded world 1
hello embedded world 2
After calling OCaml
```

The `embed_native.o` is a standalone object file that has no further references
to OCaml code beyond the runtime library, just as with the bytecode runtime.

<tip>
<title>Activating the debug runtime</title>

Despite your best efforts, it is easy to introduce a bug into C bindings that
cause heap invariants to be violated.  OCaml includes a variant of the runtime
library `libasmrun.a` that is compiled with debugging symbols.  This is
available as `libasmrund.a` and includes extra memory integrity checks upon
every garbage collection cycle.  Running these often will abort the program
near the point of corruption and helps isolate the bug.

To use this debug library, just link with `-runtime-variant d` set:

```
$ ocamlopt -runtime-variant d -verbose -o hello hello.ml hello_stubs.c
$ ./hello 
### OCaml runtime: debug mode ###
Initial minor heap size: 2048k bytes
Initial major heap size: 992k bytes
Initial space overhead: 80%
Initial max overhead: 500%
Initial heap increment: 992k bytes
Initial allocation policy: 0
Hello OCaml World!
```

If you get an error that `libasmrund.a` is not found, then this is probably
because you're using OCaml 4.00 and not 4.01.  It's only installed by default
in the very latest version, which you should be using via the `4.01.0dev+trunk`
OPAM switch.

</tip>


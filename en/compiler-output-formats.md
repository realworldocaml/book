# The Compilation Pipeline

Compiling source code into executable programs is a fairly complex process that
involves quite a few tools -- preprocessors, compilers, runtime libraries,
linkers and assemblers.  It's important how to understand how these fit
together to help with your day-to-day workflow of developing, debugging and
deploying applications.

OCaml has a strong emphasis on static type safety and rejects source code that
doesn't meet its requirements as early as possible.  The compiler does this by
running the source code through a series of checks and transformations.  Each
stage performs its job (e.g. type checking, optimization or code generation)
and discards some information from the previous stage.  The final native code
output is low-level assembly code that doesn't know anything about the OCaml
modules or objects that the compiler started with.

You don't have to do all this manually, of course.  The compiler frontends
(`ocamlc` and `ocamlopt`) run through these stages for you, controlled by the
command-line options passed to them.  Sometimes though, you'll need to dive
into the toolchain to hunt down a bug or investigate a performance problem.
This chapter explains the compiler pipeline in more depth, and the tools you
need to help visualise them.

It's even possible to compile OCaml to run efficiently on foreign environments
such as Javascript or the Java Virtual Machine.  These aren't supported by the
core OCaml distribution, but are available on OPAM.  We'll mention these as we
go through the chapter so you can experiment with them too.

In this chapter, we'll cover the following topics:

* the compilation pipeline and what each stage represents.
* source preprocessing via Camlp4 and the intermediate forms.
* the bytecode `ocamlc` compiler and `ocamlrun` interpreter.
* the native code `ocamlopt` code generator, and debugging and profiling native code.

## An overview of the toolchain

The OCaml tools accept textual source code as input with filename extensions of
`.ml` and `.mli` for modules and signatures respectively.  We explained the
basics of the build process earlier in [xref](#files-modules-and-programs), so
we'll assume you've built a few OCaml programs already by this point.

Each source file represents a *compilation unit* that is built separately.  The
compiler generates intermediate files with different filename extensions to use
as it advances through the compilation stages.  The linker takes a collection
of compiled units and produces a standalone executable or library archive that
can be re-used by other applications.

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
      /   \
     /     \ closure conversion, inlining, uncurrying,
    v       \  data representation strategy
 Bytecode    \
    |         +-----+
    |              Cmm
    |js_of_ocaml    |
    |               | code generation
    |               v
 Javascript      Assembly code
```

Notice that the pipeline branches towards the end. OCaml has multiple compiler
frontends that re-use the early stages of compilation, but produce very
different final outputs. The *bytecode interpreter* is portable and can even be
transformed into Javascript. The *native code compiler* generates specialized
executable binaries suitable for high-performance applications.

<sidebar>
<title>Obtaining the compiler source code</title>

Although it's not necessary to understand the examples, you may find it useful
to have a copy of the OCaml source tree checked out while you read through this
chapter.  The source code is available from multiple places:

* Stable releases as zip and tar archives from the [OCaml download site](http://caml.inria.fr/download.en.html).
* A Subversion anonymous mirror of the main development sources available on the [development resources](http://caml.inria.fr/ocaml/anonsvn.en.html) page online.
* A Git mirror of the Subversion repository with all the history and development branches included, browsable online at [Github](https://github.com/ocaml/ocaml).

The source tree is split up into sub-directories.  The core compiler consists of:

* `config/`: configuration directives to tailor OCaml for your operating system and architecture.
* `bytecomp/` and `byterun/`: byte-code compiler and runtime, including the garbage collector.
* `asmcomp/` and `asmrun/`: native-code compiler and runtime.  The native runtime symlinks many modules from the `byterun` directory to share code, most notably the garbage collector.
* `parsing/`: the OCaml lexer, parser and libraries for manipulating them.
* `typing/`: the static type checking implementation and type definitions.
* `camlp4/`: the source code macro preprocessor.
* `driver/`: command-line interfaces for the compiler tools.

There are a number of tools and scripts also built alongside the core compiler:

* `debugger/`: the interactive byte-code debugger.
* `toplevel/`: interactive top-level console.
* `emacs/`: a *caml-mode* for the Emacs editor.
* `stdlib/`: the compiler standard library, including the `Pervasives` module.
* `ocamlbuild/`: build system that automates common OCaml compilation modes.
* `otherlibs/`: optional libraries such as the Unix and graphics modules.
* `tools/`: command-line utilities such as `ocamldep` that are installed with the compiler.
* `testsuite/`: regression tests for the core compiler.

</sidebar>
 
We'll go through each of the compilation stages now and explain how that'll be
useful to you during day-to-day OCaml development.

## Parsing source code

When a source file is passed to the OCaml compiler, it parses the textual
source code into a more structured data type known as an Abstract Syntax Tree
(AST).  The parsing logic is implemented in OCaml itself using the techniques
described earlier in [xref](#parsing-with-ocamllex-and-menhir).  The lexer and
parser rules can be found in the `parsing` directory in the source
distribution.

The compiler emits a *syntax error* if it fails to convert the source code into
an AST.  Here's an example syntax error that we obtain by using the `module`
keyword incorrectly inside a `let` binding.

```ocaml
(* broken_module.ml *)
let _ =
  module MyString = String;
  ()
```

This fails because the `module A = B` syntax must be wrapped in a `let` binding
unless it's a top-level phrase.  Parsing immediately eliminates code which
doesn't match basic syntactic requirements, so the first example will result in
a syntax error when we run the OCaml compiler on it. 

```console
$ ocamlc -c broken_module.ml 
File "broken_module.ml", line 3, characters 2-8:
Error: Syntax error
```

The fixed version of this source code creates the `MyString` module correctly
via a local open, and compiles successfully.

```ocaml
(* fixed_module.ml *)
let _ =
  let module MyString = String in
  ()
```

The syntax error points to the line and character number of the first token
that couldn't be parsed.  In the broken example the `module` keyword isn't a
valid token at that point in parsing, so the error location information is
correct.

### Formatting source code with `ocp-indent`

Sadly, syntax errors do get more inaccurate sometimes depending on the nature
of your mistake.  Try to spot the deliberate error in the following function
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

When you compile this file you'll get a syntax error.

```console
$ ocamlc -c follow_on_function.ml
File "follow_on_function.ml", line 12, characters 0-3:
Error: Syntax error
```

The line number in the error points to the end of the `add_and_print` function,
but the actual error is at the end of the *first* function definition. There's
an extra semicolon at the end of the first definition that causes the second
definition to become part of the first `let` binding.  This eventually results
in a parsing error at the very end of the second function.

This class of bug (due to a single errant character) can be hard to spot in a
large body of code. Luckily, there's a great tool in OPAM called `ocp-indent`
that applies structured indenting rules to your source code on a line-by-line
basis. This not only beautifies your code layout, but it also makes this syntax
error much easier to locate.

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
easier to spot.  We just need to remove that semicolon and re-run `ocp-indent`
to verify that the syntax is correct.

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

The `ocp-indent` [homepage](https://github.com/OCamlPro/ocp-indent) documents
how to integrate it with your favourite editor.  All the Core libraries are
formatted using it to ensure consistency, and it's a good idea to do this
before publishing your own source code online.

### Generating documentation via OCamldoc

Whitespace and source code comments aren't significant in determining the
semantics of the program, and are removed during the parsing process.  However,
other tools in the OCaml distribution can interpret comments for their own
ends.

The OCamldoc tool uses specially formatted comments in the source code to
generate documentation bundles. These comments are combined with the function
definitions and signatures and output as structured documentation in a variety
of formats. It can generate HTML pages, LaTeX and PDF documents, UNIX manual
pages and even module dependency graphs that can be viewed using
[Graphviz](http://www.graphviz.org).

Here's a sample of some source code that's been annotated with OCamldoc
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

The OCamldoc comments are distinguished by beginning with the double asterix.
There are formatting conventions for the contents of the comment to mark
metadata.  For instance, the `@tag` fields mark specific properties such as the
author of that section of code.

Try compiling the HTML documentation and UNIX man pages by running `ocamldoc`
over the source file.

```console
$ mkdir -p html man/man3
$ ocamldoc -html -d html example.ml
$ ocamldoc -man -d man/man3 example.ml
$ man -M man Example
```

You should now have HTML files inside the `html/` directory and also be able to
view the UNIX manual pages held in `man/man3`.  There are quite a few comment
formats and options to control the output for the various backends. Refer to
the [OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html)
for the complete list.

<tip>
<title>Using custom OCamldoc generators</title>

The default HTML output stylesheets from OCamldoc are pretty spartan and
distinctly Web 1.0.  The tool supports plugging in custom documentation
generators, and there are several available that provide prettier or more
detailed output.

* [Argot](http://argot.x9c.fr/) is an enchanced HTML generator that supports
  code folding and searching by name or type definition.
* [ocamldoc-generators](https://gitorious.org/ocamldoc-generators/ocamldoc-generators) add
  support for Bibtex references within comments and generating literate
  documentation that embeds the code alongside the comments.
* JSON output is available via `odoc_json` (TODO: pull out of Xen).

</tip>

### Preprocessing with Camlp4

One powerful feature in OCaml is the facility to extend the language grammar
without having to modify the compiler. Camlp4 is a system included with OCaml
for writing extensible parsers. It provides a set of OCaml libraries that are
used to define grammars and dynamically loadable syntax extensions of such
grammars.  Camlp4 modules register new language keywords and later transform
these keywords (or indeed, any portion of the input program) into conventional
OCaml code that can be understood by the rest of the compiler.

We've already seen several examples of using Camlp4 within Core:

* `Fieldslib` to generates first-class values that represent fields of a record.
* `Sexplib` to convert types to textual s-expressions.
* `Bin_prot` for efficient binary conversion and parsing.

These all extend the language in quite a minimal way by adding a `with` keyword
to type declarations to signify that extra code should be generated from that
declaration.  For example, here's a trivial use of Sexplib and Fieldslib.

```ocaml
(* type_conv_example.ml *)
open Sexplib.Std

type t = {
  foo: int;
  bar: string
} with sexp, fields
```

Compiling this code will normally give you a syntax error if you do so without
Camlp4 since the `with` keyword isn't normally allowed after a type
definition.

```console
$ ocamlfind ocamlc -c type_conv_example.ml
File "type_conv_example.ml", line 7, characters 2-6:
Error: Syntax error
```

Now add in the syntax extension packages for `fieldslib` and `sexplib`, and
everything will compile again.

```console
$ ocamlfind ocamlc -c -syntax camlp4o -package sexplib.syntax \
  -package fieldslib.syntax type_conv_example.ml
```

We've specified a couple of additional flags here.  The `-syntax` flag directs
`ocamlfind` to add the `-pp` flag to the compiler command-line.  This flag
instructs the compiler to run the preprocessor during its parsing phase.

The `-package` flag imports other OCaml libraries. The `.syntax` suffix in the
package name is a convention that indicates these libraries are preprocessors
that should be run during parsing.  The syntax extension modules are
dynamically loaded into the `camlp4o` command which rewrites the input source
code into conventional OCaml code that has no trace of the new keywords.  The
compiler then compiles this transformed code with no knowledge of the
preprocessor's actions.

Both `fieldslib` and `sexplib` need this new `with` keyword, but they both
can't register the same extension. Instead, a library called `type_conv`
provides the common extension framework for them to use.  Type_conv registers
the `with` grammar extension to Camlp4, and the `ocamlfind` packaging ensures
that it's loaded before `variantslib` or `sexplib`.

The two extensions generate boilerplate OCaml code based on the type
definition. This avoids the inevitable performance hit of doing the code
generation dynamically. It also doesn't require a Just-In-Time (JIT) runtime
that can be a source of unpredictable dynamic behaviour.  Instead, all code is
simply generated at compile-time via Camlp4.

### Inspecting Camlp4 output

The syntax extensions accept an input AST and output a modified one.  If you're
not familiar with the Camlp4 module in question, how do you figure out what
changes it's made to your code?  The obvious way is to read the documentation
that accompanies the extension.  Another more direct approach is to use the
top-level to explore the extension's behaviour or run Camlp4 manually
yourself to see the transformation in action.

#### Using Camlp4 from the interactive top-level

The `utop` top-level can run the phrases that you type through `camlp4`
automatically. You should have at least these lines in your `~/.ocamlinit` file
in your home directory. The contents of this file are automatically executed
every time you run `utop`.  See [xref](#installation) for more details.

```
#use "topfind"
#camlp4o
```

The first directive loads the `ocamlfind` top-level interface that lets you
require `ocamlfind` packages (including all their dependent packages).  The
second directive instructs the top-level to filter all phrases via Camlp4.
You can now run `utop` and load the syntax extensions in.  We'll use the
`comparelib` syntax extension for our experiments.

OCaml provides a built-in polymorphic comparison operator that inspects the runtime
representation of two values to see if they're equal.  As we noted in
[xref](#maps-and-hashtables), the polymorphic comparison is less efficient
than defining explicit comparison functions between values. However, it quickly
become tedious to manually define comparison functions for complex type
definitions.

Let's see how `comparelib` solves this problem by running it in `utop`.

```ocaml
# #require "comparelib.syntax" ;;

# type t = { foo: string; bar : t } ;;
type t = { foo : string; bar : t; }

# type t = { foo: string; bar: t } with compare ;;
type t = { foo : string; bar : t; }
val compare : t -> t -> int = <fun>
val compare_t : t -> t -> int = <fun>
```

The first definition of `t` is a standard OCaml phrase and results in the
expected output.  The second one includes the `with compare` directive.  This
is intercepted by `comparelib` and transformed into the original type
definition with two new functions also incuded.

#### Running Camlp4 directly on the source code

The top-level is a quick way to examine the signatures generated from the
extensions, but how can we see what these new functions actually do?  You can't
do this from `utop` directly since it embeds the Camlp4 invocation as an
automated part of its operation.

Let's turn to the command-line to obtain the result of the `comparelib`
transformation instead.  Create a file that contains the type declaration from
earlier:

```ocaml
(* comparelib_test.ml *)
type t = { 
  foo: string; 
  bar: t
} with compare
```

We need to run the Camlp4 binary with the library paths to Comparelib and
Type_conv.  Let's use a small shell script to wrap this invocation.

```bash
#!/bin/sh
# camlp4_dump

OCAMLFIND="ocamlfind query -predicates syntax,preprocessor -r"
INCLUDE=`$OCAMLFIND -i-format comparelib.syntax`
ARCHIVES=`$OCAMLFIND -a-format comparelib.syntax`
camlp4o -printer o $INCLUDE $ARCHIVES $1
```

The script uses the `ocamlfind` package manager to list the include and library
paths needed by `comparelib`.  It then invokes the `camlp4o` preprocessor with
these paths and outputs the resulting AST to the standard output.


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

The output contains the original type definition accompanied by some
automatically generated code that implements an explicit comparison function
for each field in the record.  If you're using the extension in your compiler
command-line, this generated code is then compiled as if you had typed it in
yourself.

Another useful feature of `type_conv` is that it can generate module signatures
too.  Copy the earlier type definition into a `comparelib_test.mli` and rerun
the Camlp4 dumper script.

```console
$ ./camlp4_dump.sh test_comparelib.mli 
type t = { foo : string; bar : t }

val compare : t -> t -> int
```

The external signature generated by `comparelib` is much simpler than the
actual code.  Running Camlp4 directly on the original source code lets you
see these all these transformations precisely.

<note>
<title>Don't overdo the syntax extensions</title>

Syntax extensions are a powerful extension mechanism that can completely alter
your source code's layout and style.  Core includes a very conservative set of
extensions that take care to minimise the syntax changes.  There are a number
of third-party libraries that are much more ambitious -- some introduce
whitespace-sensitive indentation while others build entirely new embedded
languages using OCaml as a host language.

While it's tempting to compress all your boiler-plate code into Camlp4
extensions, it can make your source code much harder for other people to
quickly read and understand.  Core mainly focuses on type-driven code
generation using the `type_conv` extension and doesn't fundamentally change the
OCaml syntax.

Another thing to consider before deploying your own syntax extension is
compatibility with other extensions.  Two separate extensions can create a
grammar clash that leads to odd syntax errors and hard-to-reproduce bugs.
That's why most of Core's syntax extensions go through `type_conv`, which acts
as a single point for extending the grammar via the `with` keyword.

</note>

We've deliberately only shown you how to use Camlp4 extensions here, and not
how to build your own. The full details of building new extensions are fairly
daunting and could be the subject of an entirely new book.

The best resources to get started are the online [Camlp4 wiki](http://brion.inria.fr/gallium/index.php/Camlp4), and
using OPAM to obtain existing Camlp4 extensions and inspecting their source code.
A series of [blog posts](http://ambassadortothecomputers.blogspot.co.uk/p/reading-camlp4.html)
by Jake Donham also serve as a useful guide to the internals of syntax extensions.

## Static type checking

After obtaining a valid abstract syntax tree, the compiler then verifies that
the code obeys the rules of the static type system. Code that is syntactically
correct but misuses values is rejected with an explanation of the problem.

Static type checking is split into two logical phases in OCaml.  The core
language has a sophisticated type inference engine that automatically figures
out types for your code without you having to write them out by hand.  The
module language lets you group functions together and explicitly manipulate
and re-use them.

While the core language has a strong emphasis on automatic inference, the
module language is much more explicit about matching signatures and
implementations against each other. You can consider type inference well suited
to local bits of code (i.e. individual modules), and modules to establish
abstraction boundaries between components.  The module system scales up to the
needs of large-scale software engineering -- some of the larger OCaml
code-bases contain thousands of files and modules.

You can explore basic type inference very easily from the top-level, or by
asking the compiler to display the types it infers. Create a file with a single
type definition and value.

```ocaml
(* typedef.ml *)
type t = Foo | Bar
let v = Foo
```

Now run the compiler with the `-i` flag to infer the types for the compilation
unit. This runs the type checker but doesn't compile it any further after
displaying the interface to the standard output.

```console
$ ocamlc -i typedef.ml
type t = Foo | Bar
val v : t
```

For a file, the output is the default signature for the module.  It's often useful to
redirect this output to an `mli` file to give you a starting signature to edit
the external interface without having to type it all in by hand.  The compiler
also stores a compiled version of the interface as a `cmi` file.  This interface
is either obtained from compiling an `mli` signature file for a module, or 
by the inferred type if there is only an `ml` implementation present.

If you have a mismatch between the `ml` and `mli` signatures, the type-checker
will give you an immediate error. 

```console
$ echo type t = Foo > test.ml
$ echo type t = Bar > test.mli
$ ocamlc -c test.mli test.ml
File "test.ml", line 1:
Error: The implementation test.ml does not match the interface test.cmi:
       Type declarations do not match:
         type t = Foo
       is not included in
         type t = Bar
       File "test.ml", line 1, characters 5-12: Actual declaration
       Their first fields have different names, Foo and Bar.
```

It's good coding style to have an explicit `mli` signature file as this is also
where you can put OCamldoc comments as documentation for that module.  Since
the compiler ensures that the external signature matches your implementation,
it adds very little maintenance burden.

### The type inference process

Type inference is the process of determining the appropriate types for
expressions based on their use.  It's a feature that's partially present in
many other languages such as Haskell and Scala, but OCaml embeds it as a
fundamental feature throughout the core language.

OCaml type inference is based on the Hindley-Milner algorithm, which is notable
for its ability to infer the most general type for an expression without
requiring any explicit type annotations.  The algorithm can deduce multiple
types for an expression, and has the notion of a *principal type* that is the
most general choice from the possible inferences.  Manual type annotations can
always specialize the type explicitly, but the automatic inference selects the
most general (or "principal") type.

OCaml does has some language extensions which strain the limits of principal
type inference, but by and large most programs you write will never *require*
annotations (although they sometimes help the compiler produce better error
messages).

<sidebar>
<title>Enforcing principal typing</title>

You can also pass a `-principal` flag to the compiler to turn on a stricter
principal type checking mode.  This detects risky uses of type information to
ensure that the type inference has one principal result.  A type is considered
risky if the success or failure of type inference depends on the order in which
sub-expressions are typed.

The principality check only affects a few language features:

* polymorphic methods for objects.
* permuting the order of labeled arguments in a function from their type definition.
* discarding optional labelled arguments.
* generalized algebraic data types (GADTs) present from OCaml 4.0 onwards.
* automatic disambiguation of record field and constructor names (since OCaml 4.1)

Here's an example of principality warnings when used with polymorphic methods.

```console
$ utop -principal
# type t = < id: 'a. 'a -> 'a >;;
type t = < id : 'a. 'a -> 'a >

# let f (x : t) = x, x#id;;  
val f : t -> t * ('a -> 'a) = <fun>
(* safe code: the type of x is known at all its use points *)

# let f x = (x : t), x#id;;
Warning 18: this use of a polymorphic method is not principal.
val f : t -> t * ('a -> 'a) = <fun>
(* unsafe code: the type is only known because typing goes from left to right *)

# let f x = x#id, (x : t);;
Error: This expression has type < id : 'a; .. >
      but an expression was expected of type t
      The universal variable 'a0 would escape its scope
(* just exchanging the members of the pair causes a failure *)
```

Ideally, all code should systematically use `-principal`.  It reduces variance
in type inference and enforces the notion of a single known type.  However,
there are drawbacks to this mode: type inference is slower and the `cmi` files
become larger.  This is generally only a problem if you use objects
extensively, which usually have larger type signature to cover all their
methods.

As a result, the suggested approach is to only compile with `-principal`
occasionally to check if your code is compliant.  If compiling in principal
mode works, it is guaranteed that the program will passing type checking in
non-principal mode too.

Bear in mind that the `cmi` files generated in principal mode differ from the
default mode. Try to ensure that you compile your whole project with it
activated.  Getting the files mixed up won't let you violate type safety, but
can result in the type checker failing unexpectedly very occasionally.  In this
case, just recompile with a clean source tree.

</sidebar>

### Separate compilation and module search paths

Modules are most useful for large applications which consist of many files (or
*compilation units*). Modules let each file be compiled separately, thus
minimizing recompilation after changes.  Compilation units are simply special
cases of OCaml modules and signatures, and the relationship between the units
can be explained in terms of the module system.

Create a file called `alice.ml` with the following contents.

```ocaml
(* alice.ml *)
let friends = [ Bob.name ]
```

and a corresponding signature file.

```ocaml
(* alice.mli *)
val friends : Bob.t list
```

These two files are exactly analogous to including this code directly in another
module that references `Alice`.

```ocaml
module Alice : sig
  val friends : Bob.t list
end = struct
  let friends = [ Bob.name ]
end
```

However, `Alice` also has a reference to another module `Bob`, which must
contain at least a `Bob.name` value and define a `Bob.t` type.

The type checker needs to resolve such external module references into concrete
structures and signatures in order to unify types across module boundaries.  It
does this by searching a list of directories for a compiled interface file
matching that module's name.  For example, it will look for `alice.cmi` and
`bob.cmi` on the search path, and use the first ones it encounters as the
interfaces for `Alice` and `Bob`.

You can set the search path by adding the `-I` flag to the compiler
command-line.  This can get complex when you have lots of libraries, and is the
reason why the `ocamlfind` frontend to the compiler exists.  OCamlfind
automates the process of turning third-party package names into concrete `-I`
flags that are added to the compiler invocation.

By default, only the current directory and the OCaml standard library will be
searched for `cmi` files.  The `Pervasives` module from the standard library
will also be opened by default in every compilation unit.  The standard library
location is obtained by running `ocamlc -where`, and can be overridden by
setting the `CAMLLIB` environment variable.  Needless to say, don't override
the default unless you have a good reason to, such as setting up a
cross-compilation environment.

<sidebar>
<title>Inspecting compilation units with `ocamlobjinfo`</title>

For separate compilation to be sound, we need to ensure that all the `cmi`
files used to type-check a module are the same across compilation runs.  If
they vary, this raises the possibility of two modules checking different type
signature for a common module with the same name.  This in turn lets the
program completely violate the static type system and can lead to memory
corruption and crashes.

OCaml guards against this by recording a CRC checksum in every `cmi`.  Let's
examine our earlier `typedef.ml` more closely.

```console
$ ocamlc -c typedef.ml
$ ocamlobjinfo typedef.cmi
File typedef.cmi
Unit name: Typedef
Interfaces imported:
	559f8708a08ddf66822f08be4e9c3372	Typedef
	65014ccc4d9329a2666360e6af2d7352	Pervasives
```

`ocamlobjinfo` examines the compiled interface and displays what other
compilation units it depends on.  In this case, we don't use any external
modules other than `Pervasives`.  Every module depends on `Pervasives` by
default, unless you use the `-nopervasives` flag (this is an advanced use-case,
and you shouldn't normally need it).

The long alphanumeric identifier beside each module name is a hash calculated
from all the types and values exported from that compilation unit.  It's used
during type-checking and linking to ensure that all of the compilation units
have been compiled consistently against each other.  A difference in the hashes
means that a compilation unit with the same module name may have conflicting
type signatures in different modules.  The compiler will reject such programs
with an error similar to this:

```console
File "foo.ml", line 1, characters 0-1:
Error: The files /home/build/bar.cmi
       and /usr/lib/ocaml/map.cmi make inconsistent assumptions
       over interface Map
```

This hash check is very conservative, but ensures that separate compilation
remains type-safe all the way up to the final link phase.  Your build system
should ensure that you never see the error messages above, but if you do run
into it, just clean out your intermediate files and recompile from scratch.

</sidebar>

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

## The untyped lambda form

Once OCaml has passed the type checking stage, it can stop emitting syntax
and type errors and begin the process of compiling the well-formed modules
into executable code.

The next stage eliminates all the static type information into a simpler
intermediate *lambda form*.  The lambda form discards higher-level constructs
such as modules and objects and replaces them with simpler values such as
records and function pointers.  Pattern matches are also analyzed and compiled
into highly optimized automata.

The lambda form is the key stage that discards the OCaml type information and
maps the source code to the runtime memory model described in
[xref](#memory-representation-of-values).  This stage also performs some
optimizations, most notably converting pattern match statements into more
optimized but low-level statements. 

### Pattern matching optimization

The compiler dumps the lambda form in an s-expression syntax if you add the
`-dlambda` directive to the command-line.  Let's use this to learn more about
how the OCaml pattern matching engine works by building three different pattern
matches and comparing their lambda forms.

Let's start by creating a straightforward exhaustive pattern match using normal
variants.

```ocaml
(* pattern_monomorphic_exhaustive.ml *)
type t = | Alice | Bob | Charlie | David

let test v =
  match v with
  | Alice   -> 100
  | Bob     -> 101
  | Charlie -> 102
  | David   -> 103
```

The lambda output for this code looks like this.

```console
$ ocamlc -dlambda -c pattern_monomorphic_exhaustive.ml
(setglobal Pattern_monomorphic_exhaustive!
  (let
    (test/1013
       (function v/1014
         (switch* v/1014
          case int 0: 100
          case int 1: 101
          case int 2: 102
          case int 3: 103)))
    (makeblock 0 test/1013)))
```

It's not important to understand every detail of this internal form, but
some interesting points are:

* There are no mention of modules or types any more.  Global values are
  created via `setglobal` and OCaml values are constructed by `makeblock`.  The
  blocks are the runtime values you should remember from [xref](#memory-representation-of-values).
* The pattern match has turned into a switch case that jumps to the right case
  depending on the header tag of `v`.  Recall that variants without parameters are stored 
  in memory as integers in the order which they appear.  The pattern matching 
  engine knows this and has transformed the pattern into an efficient jump table. 
* Values are addressed by a unique name that distinguished shadowed values by appending
  a number (e.g. `v/1014`). The type safety checks in the earlier phase ensure that
  these low-level accesses never violate runtime memory safety, so this layer
  doesn't do any dynamic checks.  Unwise use of unsafe features such as the
  `Obj.magic` module can still easily induce crashes at this level.

The first pattern match is *exhaustive*, so there are no other cases that the
comipler needed to worry about.  What happens if we make the same code use an
incomplete pattern match?

```ocaml
(* pattern_monomorphic_incomplete.ml *)
type t = | Alice | Bob | Charlie | David

let test v =
  match v with
  | Alice   -> 100
  | Bob     -> 101
  | _       -> 102
```

The lambda output for this code is now quite different.

```console
$ ocamlc -dlambda -c pattern_monomorphic_incomplete.ml 
(setglobal Pattern_monomorphic_incomplete!
  (let
    (test/1013
       (function v/1014 (if (!= v/1014 1) (if (!= v/1014 0) 102 100) 101)))
    (makeblock 0 test/1013)))
```

The compiler has now reverted to testing the value as a set of nested
conditionals.  The lambda code above checks to see if the value is `Alice`,
then if it's `Bob`, and finally falls back to the default `102` return value
for everything else.  Using exhaustive pattern matches is thus a better coding
style at several levels: it rewards you with more useful compile-time warnings
when you modify type definitions *and* generates more efficient runtime code too.

Finally, let's look at the same example with polymorphic variants instead of
normal variants.

```ocaml
(* pattern_polymorphic.ml *)
let test v =
  match v with
  | `Alice   -> 100
  | `Bob     -> 101
  | `Charlie -> 102
  | `David   -> 103
```

The lambda form for this is the most inefficient result yet.

```console
$ ocamlc -dlambda -c pattern_polymorphic.ml 
(setglobal Pattern_polymorphic!
  (let
    (test/1008
       (function v/1009
         (if (>= v/1009 482771474) (if (>= v/1009 884917024) 100 102)
           (if (>= v/1009 3306965) 101 103))))
    (makeblock 0 test/1008)))
```

We mentioned earlier in [xref](#variants) that pattern matching over
polymorphic variants is slightly less efficient, and it should be clearer why
this is the case now.  Polymorphic variants have a runtime value that's
calculated by hashing the variant name, and so the compiler just tests each of
these possible values in sequence.

### Benchmarking pattern matching implementations

Let's benchmark these three pattern matching techniques to quantify their
runtime costs better.  The `Core_bench` module runs the tests thousands of
times and also calculates statistical variance of the results.  You'll need to
`opam install core_bench` to get the library.

```ocaml
(* pattern.ml: benchmark different pattern matching styles *)
open Core.Std
open Core_bench.Std

type t = | Alice | Bob | Charlie | David

let polymorphic_pattern () =
  let test v =
    match v with
    | `Alice   -> 100
    | `Bob     -> 101
    | `Charlie -> 102
    | `David   -> 103
  in
  List.iter ~f:(fun v -> ignore(test v))
    [`Alice; `Bob; `Charlie; `David]
 
let monomorphic_pattern_exhaustive () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101
    | Charlie -> 102
    | David   -> 103
  in
  List.iter ~f:(fun v -> ignore(test v))
    [ Alice; Bob; Charlie; David ]

 let monomorphic_pattern_incomplete () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101
    | _       -> 102
  in
  List.iter ~f:(fun v -> ignore(test v))
    [ Alice; Bob; Charlie; David ]
 
let tests = [
    "Polymorphic pattern", polymorphic_pattern;
    "Monomorphic incomplete pattern", monomorphic_pattern_incomplete;
    "Monomorphic exhaustive pattern", monomorphic_pattern_exhaustive
]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run
```

Building and executing this example will run for around 30 seconds by default,
and you'll see the results summarised in a neat table.

```console
$ ocamlbuild -use-ocamlfind -package core -package core_bench -tag thread pattern.native
Estimated testing time 30s (change using -quota SECS).
┌────────────────────────────────┬───────────┬─────────────┬────────────┐
│ Name                           │ Time (ns) │   Time 95ci │ Percentage │
├────────────────────────────────┼───────────┼─────────────┼────────────┤
│ Polymorphic pattern            │     22.38 │ 22.34-22.43 │     100.00 │
│ Monomorphic incomplete pattern │     20.98 │ 20.95-21.02 │      93.77 │
│ Monomorphic exhaustive pattern │     19.53 │ 19.49-19.58 │      87.25 │
└────────────────────────────────┴───────────┴─────────────┴────────────┘
```

These numbers confirm our earlier performance hypothesis obtained from
inspecting the lambda code. The shortest running time comes from the exhaustive
pattern match and polymorphic variant pattern matching is the slowest.  There
isn't a hugely significant difference in these examples, but you can use the
same techniques to peer into the innards of your own source code and narrow
down any performance hotspots.

The lambda form is primarily a stepping stone to the bytecode executable format
that we'll cover next.  It's often easier to look at the textual output from
this stage than to wade through the native assembly code from compiled
executables.

<note>
<title>Learning more about pattern matching compilation</title>

Pattern matching is a fundamental part of OCaml programming. You'll often
encounter deeply nested pattern matches over complex data structures in real
code.  A good paper that describes the fundamental algorithms implemented in
OCaml is the ["Optimizing pattern matching"](http://dl.acm.org/citation.cfm?id=507641) ICFP 2001 paper by Fabrice
Le Fessant and Luc Maranget.

The paper describes the backtracking algorithm used in classical pattern
matching compilation, and also several OCaml-specific optimizations such as the
use of exhaustiveness information and control flow optimizations via static
exceptions.

It's not essential that you understand all of this just to use pattern matching
of course, but it'll give you insight as to why pattern matching is such a
lightweight language construct to use in OCaml code.

</note>

## Generating portable bytecode

After the lambda form has been generated, we are very close to having
executable code.  The OCaml tool-chain branches into two separate compilers at
this point.  We'll describe the the `ocamlc` bytecode compiler first, which
consists of two pieces:

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

### Compiling OCaml bytecode 

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

### Linking OCaml bytecode

The bytecode runtime comprises three parts: the bytecode interpreter, garbage
collector, and a set of C functions that implement the primitive operations.
Bytecode instructions are provided to call these C functions.  The OCaml linker
produces bytecode for the standard runtime system by default, and any custom C
functions in your code (e.g. from C bindings) require the runtime to
dynamically load a shared library.

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

#### Embedding OCaml bytecode in C code

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

The native code compiler is ultimately the tool that most production OCaml code
goes through.  It compiles the lambda form into fast native code executables,
with cross-module inlining and additional optimization passes that the bytecode
interpreter doesn't perform.  Care is taken to ensure compatibility with the
bytecode runtime, so the same code should run identically when compiled with
either toolchain.

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

### Inspecting assembly output

The native code compiler generates assembly language that is then passed to the
system assembler for compiling into object files.  You can get `ocamlopt` to
output the assembly by passing the `-S` flag to the compiler command-line.

The assembly code is highly architecture specific, so the discussion below
assumes an Intel or AMD 64-bit platform.  We've generated the example code
using `-inline 20` and `-nodynlink` since it's best to generate assembly code
with the full optimizations that the compiler supports. Even though these
optimizations make the code a bit harder to read, it will give you a more
accurate picture of what executes on the CPU.  Don't forget that you can use
the lambda code from earlier to get a slightly higher level picture of the code
if you get lost in the more verbose assembly.

#### The impact of polymorphic comparison

We warned you earlier in [xref](#maps-and-hashtables) that using polymorphic
comparison is both convenient and perilous.  Let's look at precisely what
the difference is at the assembly language level now.

First create a comparison function where we've explicitly annotated
the types, so the compiler knows that only integers are being compared.

```ocaml
(* compare_mono.ml *)
let cmp (a:int) (b:int) =
  if a > b then a else b
```

Now compile this into assembly and read the resulting `compare_mono.S` file.

```console
$ ocamlopt -inline 20 -nodynlink -S compare_mono.ml
$ cat compare_mono.S
```

If you've never seen assembly language before then the contents may be rather
scary.  While you'll need to learn x86 assembly to fully understand it, we'll
try to give you some basic instructions to spot patterns in this section.  The
excerpt of the implementation of the `cmp` function can be found below.

```
_camlCompare_mono__cmp_1008:
	.cfi_startproc
.L101:
	cmpq	%rbx, %rax
	jle	.L100
	ret
	.align	2
.L100:
	movq	%rbx, %rax
	ret
	.cfi_endproc
```

The `_camlCompare_mono__cmp_1008` is an assembly label that has been computed
from the module name (`Compare_mono`) and the function name (`cmp_1008`).  The
numeric suffix for the function name comes straight from the lambda form (which
you can inspect using `-dlambda`, but in this case isn't necessary).

The arguments to `cmp` are passed in the `%rbx` and `%rax` registers, and
compared using the `jle` "jump if less than or equal" instruction.  This
requires both the arguments to be immediate integers to work.  Now let's see
what happens if our OCaml code omits the type annotations and is a polymorphic
comparison instead.

```ocaml
(* compare_poly.ml *)
let cmp a b =
  if a > b then a else b
```

Compiling this code with `-S` results in a significantly more complex assembly
output for the same function.

```
_camlCompare_poly__cmp_1008:
        .cfi_startproc
        subq    $24, %rsp
        .cfi_adjust_cfa_offset  24
.L101:
        movq    %rax, 8(%rsp)
        movq    %rbx, 0(%rsp)
        movq    %rax, %rdi
        movq    %rbx, %rsi
        leaq    _caml_greaterthan(%rip), %rax
        call    _caml_c_call
.L102:
        leaq    _caml_young_ptr(%rip), %r11
        movq    (%r11), %r15
        cmpq    $1, %rax
        je      .L100
        movq    8(%rsp), %rax
        addq    $24, %rsp
        .cfi_adjust_cfa_offset  -24
        ret
        .cfi_adjust_cfa_offset  24
        .align  2
.L100:
        movq    0(%rsp), %rax
        addq    $24, %rsp
        .cfi_adjust_cfa_offset  -24
        ret
        .cfi_adjust_cfa_offset  24
        .cfi_endproc
```

The `.cfi` directives are assembler hints that contain Call Frame Information
that lets the GNU debugger provide more sensible backtraces, and have no effect
on runtime performance.  Notice that the rest of the implementation is no
longer a simple register comparison.  Instead, the arguments are pushed on the
stack (the `%rsp` register) and a C function call is invoked by placing a
pointer to `caml_greaterthan` in `%rax` and jumping to `caml_c_call`.

OCaml on 64-bit Intel architectures caches the location of the minor heap in
the `%r11` register since it's so frequently referenced in OCaml functions.
This register isn't guaranteed to be preserved when calling into C code (which
can clobber `%r11` for its own purposes), and so `%r11` is restored after
returning from the `caml_greaterthan` call.  Finally the return value of the
comparison is popped from the stack and returned.

<tip>
<title>The implementation of `compare_val` in the runtime</title>

The ... TODO

</tip>

You don't have to fully understand the intricacies of assembly language to see
that this polymorphic comparison is much heavier than the simple monomorphic
integer comparison from earlier.  Let's confirm this hypothesis again by
writing a quick `Core_bench` test with both functions.

```ocaml
$ cat bench_poly_and_mono.ml 
open Core.Std
open Core_bench.Std

let polymorphic_compare () =
  let cmp a b = if a > b then a else b in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let monomorphic_compare () =
  let cmp (a:int) (b:int) =
    if a > b then a else b in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let tests = [
    "Polymorphic comparison", polymorphic_compare;
    "Monomorphic comparison", monomorphic_compare ]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run
```

Running this shows quite a significant runtime difference between the two.

```console
$ ./bench_poly_and_mono.native 
Estimated testing time 20s (change using -quota SECS).
┌────────────────────────┬───────────┬───────────────┬────────────┐
│ Name                   │ Time (ns) │     Time 95ci │ Percentage │
├────────────────────────┼───────────┼───────────────┼────────────┤
│ Polymorphic comparison │    10_087 │ 10_080-10_096 │     100.00 │
│ Monomorphic comparison │    585.51 │ 584.60-586.57 │       5.80 │
└────────────────────────┴───────────┴───────────────┴────────────┘
```

We see that the polymorphic comparison is close to 20 times slower!  These
results shouldn't be taken too seriously as this is a very narrow test, which
like all such microbenchmarks aren't representative of more complex codebases.
However, if you're building numerical code that runs many iterations in a tight
inner loop, it's worth manually peering at the produced assembly code to see if
you can hand-optimize it.

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


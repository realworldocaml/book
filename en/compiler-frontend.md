# The Compiler Frontend: Parsing and Type Checking

Compiling source code into executable programs is a fairly complex process that
involves quite a few tools -- preprocessors, compilers, runtime libraries,
linkers and assemblers.  It's important to understand how these fit together to
help with your day-to-day workflow of developing, debugging and deploying
applications.

OCaml has a strong emphasis on static type safety and rejects source code that
doesn't meet its requirements as early as possible.  The compiler does this by
running the source code through a series of checks and transformations.  Each
stage performs its job (e.g. type checking, optimization or code generation)
and discards some information from the previous stage.  The final native code
output is low-level assembly code that doesn't know anything about the OCaml
modules or objects that the compiler started with.

You don't have to do all this manually, of course.  The compiler frontends
(`ocamlc` and `ocamlopt`) are invoked via the command-line and chain the stages
together for you.  Sometimes though, you'll need to dive into the toolchain to
hunt down a bug or investigate a performance problem.  This chapter explains
the compiler pipeline in more depth so you understand how to harness the
command-line tools effectively.

In this chapter, we'll cover the following topics:

* the compilation pipeline and what each stage represents.
* source preprocessing via Camlp4 and the intermediate forms.
* the type-checking process, including module resolution.

The details of the compilation process into executable code can be found next
in [xref](#the-compiler-backend-byte-code-and-native-code).

## An overview of the toolchain

The OCaml tools accept textual source code as input, using filename extensions
of `.ml` and `.mli` for modules and signatures respectively.  We explained the
basics of the build process earlier in [xref](#files-modules-and-programs), so
we'll assume you've built a few OCaml programs already by this point.

Each source file represents a *compilation unit* that is built separately.  The
compiler generates intermediate files with different filename extensions to use
as it advances through the compilation stages.  The linker takes a collection
of compiled units and produces a standalone executable or library archive that
can be reused by other applications.

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
    |ocamlrun       |
    |               | code generation
    |               | assembly & linking
    v               v
 Interpreted    Compiled
```

Notice that the pipeline branches towards the end. OCaml has multiple compiler
frontends that reuse the early stages of compilation, but produce very
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

When a source file is passed to the OCaml compiler, its first task is to parse
the text into a more structured Abstract Syntax Tree (AST). The parsing logic
is implemented in OCaml itself using the techniques described earlier in
[xref](#parsing-with-ocamllex-and-menhir).  The lexer and parser rules can be
found in the `parsing` directory in the source distribution.

### Syntax errors

The OCaml parser's goal is to output a well-formed AST data structure to the
next phase of compilation, and so it rejects any source code that doesn't match
basic syntactic requirements.  The compiler emits a *syntax error* in this
situation, with a pointer to the filename and line and character number that's
as close to the error as possible.

Here's an example syntax error that we obtain by performing a module assignment
as a statement instead of as a let-binding.

```ocaml
(* broken_module.ml *)
let _ =
  module MyString = String;
  ()
```

The above code results in a syntax error when compiled.

```console
$ ocamlc -c broken_module.ml 
File "broken_module.ml", line 3, characters 2-8:
Error: Syntax error
```

The correct version of this source code creates the `MyString` module correctly
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

### Automatically indenting source code

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
large body of code. Luckily, there's a great tool in OPAM called ocp-indent
that applies structured indenting rules to your source code on a line-by-line
basis. This not only beautifies your code layout, but it also makes this syntax
error much easier to locate.

Let's run our erronous file through ocp-indent and see how it processes it.

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
easier to spot.  We just need to remove that semicolon and re-run ocp-indent
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

The [ocp-indent homepage](https://github.com/OCamlPro/ocp-indent) documents
how to integrate it with your favourite editor.  All the Core libraries are
formatted using it to ensure consistency, and it's a good idea to do this
before publishing your own source code online.

### Generating documentation from interfaces

Whitespace and source code comments are removed during parsing and aren't
significant in determining the semantics of the program.  However, other tools
in the OCaml distribution can interpret comments for their own ends.

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

The OCamldoc comments are distinguished by beginning with the double
asterisk.  There are formatting conventions for the contents of the
comment to mark metadata.  For instance, the `@tag` fields mark
specific properties such as the author of that section of code.

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
* JSON output is available via a custom [generator](https://github.com/xen-org/ocamldoc-json)
  in Xen.

</tip>

## Preprocessing source code

One powerful feature in OCaml is a facility to extend the standard language
grammar without having to modify the compiler.  You can roughly think of it as
a type-safe version to the `cpp` preprocessor used in C/C++ to control
conditional compilation directives.

The OCaml distribution includes a system called Camlp4 for writing extensible
parsers. This provides some OCaml libraries that are used to define grammars
and also dynamically loadable syntax extensions of such grammars.  Camlp4
modules register new language keywords and later transform these keywords (or
indeed, any portion of the input program) into conventional OCaml code that can
be understood by the rest of the compiler.

We've already seen several Core libraries that use Camlp4:

* `Fieldslib` generates first-class values that represent fields of a record.
* `Sexplib` to convert types to textual s-expressions.
* `Bin_prot` for efficient binary conversion and parsing.

These libraries all extend the language in quite a minimal way by adding a
`with` keyword to type declarations to signify that extra code should be
generated from that declaration.  For example, here's a trivial use of Sexplib
and Fieldslib.

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

Both Fieldslib and Sexplib need this new `with` keyword, but they both
can't register the same extension. Instead, a library called Type_conv
provides the common extension framework for them to use.  Type_conv registers
the `with` grammar extension to Camlp4, and the OCamlfind packaging ensures
that it's loaded before Variantslib or Sexplib.

The two extensions generate boilerplate OCaml code based on the type
definition. This avoids the inevitable performance hit of doing the code
generation dynamically. It also doesn't require a Just-In-Time (JIT) runtime
that can be a source of unpredictable dynamic behaviour.  Instead, all code is
simply generated at compile-time via Camlp4.

### Using Camlp4 interactively

The syntax extensions accept an input AST and output a modified one.  If you're
not familiar with the Camlp4 module in question, how do you figure out what
changes it's made to your code?  The obvious way is to read the documentation
that accompanies the extension.

Another approach is to use the top-level to explore the extension's behaviour
or run Camlp4 manually yourself to see the transformation in action.  We'll
show you how to do both of these now.

#### Using Camlp4 from the interactive top-level

The `utop` top-level can run the phrases that you type through `camlp4`
automatically. You should have at least these lines in your `~/.ocamlinit` file
in your home directory (see [xref](#installation) for more information).

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

### Running Camlp4 from the command-line

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

<caution>
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

</caution>

### Further reading on Camlp4

We've deliberately only shown you how to use Camlp4 extensions here, and not
how to build your own. The full details of building new extensions are fairly
daunting and could be the subject of an entirely new book.

The best resources to get started are:

* the online [Camlp4 wiki](http://brion.inria.fr/gallium/index.php/Camlp4).
* using OPAM to install existing Camlp4 extensions and inspecting their source code.
* a series of [blog posts](http://ambassadortothecomputers.blogspot.co.uk/p/reading-camlp4.html) by
Jake Donham describe the internals of Camlp4 and its syntax extension mechanism.

## Static type checking

After obtaining a valid abstract syntax tree, the compiler has to verify that
the code obeys the rules of the OCaml type system. Code that is syntactically
correct but misuses values is rejected with an explanation of the problem.

Although type checking is done in a single pass in OCaml, it actually consists
of three distinct steps that happen simultaneously:

* an *automatic type inference* algorithm that calculates types
  for a module without requiring manual type annotations.
* a *module system* that combines software components with explicit
  knowledge of their type signatures.
* performing *explicit subtyping* checks for objects and polymorphic variants.

Automatic type inference lets you write succinct code for a particular task and
have the compiler ensure that your use of variables is locally consistent.

Type inference doesn't scale to very large code bases that depend on separate
compilation of files.  A small change in one module may ripple through
thousands of other files and libraries and require all of them to be
recompiled.  The module system solves this by providing the facility to combine
and manipulate explicit type signatures for modules within a large project, and
also to reuse them via functors and first-class modules.

Subtyping in OCaml objects is always an explicit operation (via the `:>`
operator).  This means that it doesn't complicate the core type inference
engine and can be tested as a separate concern.

### Displaying inferred types from the compiler

We've already seen how you can explore type inference directly from the
top-level.  It's also possible to generate type signatures for an entire file
by asking the compiler to do the work for you. Create a file with a single type
definition and value.

```ocaml
(* typedef.ml *)
type t = Foo | Bar
let v = Foo
```

Now run the compiler with the `-i` flag to infer the type signature for that
file.  This runs the type checker but doesn't compile the code any further
after displaying the interface to the standard output.

```console
$ ocamlc -i typedef.ml
type t = Foo | Bar
val v : t
```

The output is the default signature for the module which represents the input
file.  It's often useful to redirect this output to an `mli` file to give you a
starting signature to edit the external interface without having to type it all
in by hand.

The compiler stores a compiled version of the interface as a `cmi` file.  This
interface is either obtained from compiling an `mli` signature file for a
module, or by the inferred type if there is only an `ml` implementation
present.

The compiler makes sure that your `ml` and `mli` files have compatible
signatures. The type checker throws an immediate error if this isn't the case. 

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

<note>
<title>Which comes first: the `ml` or the `mli`?</title>

There are two schools of thought on which order OCaml code should be written
in.  It's very easy to begin writing code by starting with an `ml` file and
using the type inference to guide you as you build up your functions.  The
`mli` file can then be generated as described above, and the exported functions
documented.

If you're writing code that spans multiple files, it's sometimes easier
to start by writing all the `mli` signatures and checking that they type check
against each other.  Once the signatures are in place, you can write the
implementations with the confidence that they'll all glue together correctly
with no cyclic dependencies between the modules.

As with any such stylistic debate, you should experiment with which system
works best for you.  Everyone agrees on one thing though: no matter what order
you write them, production code should always explicitly define an `mli` file
for every `ml` file in the project.

Signature files provide a place to write succinct documentation and to abstract
internal details that shouldn't be exported.  Maintaining separate signature
files also speeds up incremental compilation in larger code-bases, since
recompiling a `mli` signature is much faster than a full compilation of the
implementation to native code.

</note>

### Type inference

Type inference is the process of determining the appropriate types for
expressions based on their use.  It's a feature that's partially present in
many other languages such as Haskell and Scala, but OCaml embeds it as a
fundamental feature throughout the core language.

OCaml type inference is based on the Hindley-Milner algorithm, which is notable
for its ability to infer the most general type for an expression without
requiring any explicit type annotations.  The algorithm can deduce multiple
types for an expression, and has the notion of a *principal type* that is the
most general choice from the possible inferences.  Manual type annotations can
specialize the type explicitly, but the automatic inference selects the most
general type unless told otherwise.

OCaml does have some language extensions which strain the limits of principal
type inference, but by and large most programs you write will never *require*
annotations (although they sometimes help the compiler produce better error
messages).

#### Adding type annotations to find errors

It's often said that the hardest part of writing OCaml code is getting past the
type checker -- but once the code does compile, it works correctly the first
time! 

There are a couple of tricks to make it easier to quickly locate type errors in
your code. The first is to introduce manual type annotations to narrow down the
source of your error more accurately.  These annotations shouldn't actually
change your types and can be removed once your code is correct. However, they
act as anchors to locate errors while you're still writing your code.

Manual type annotations are particulary useful if you use lots of
polymorphic variants or objects.  Type inference with row polymorphism can
generate some very large signatures, and errors tend to propagate more widely
than if you are using more explicitly typed variants or classes.

For instance, consider this broken example that expresses some simple 
algebraic operations over integers.

```ocaml
(* broken_poly.ml *)

let rec algebra =
  function
  | `Add (x,y) -> (algebra x) + (algebra y)
  | `Sub (x,y) -> (algebra x) - (algebra y)
  | `Mul (x,y) -> (algebra x) * (algebra y)
  | `Num x     -> x

let _ =
  algebra (
    `Add (
      (`Num 0),
      (`Sub (
          (`Num 1),
          (`Mul (
              (`Nu 3),(`Num 2)
            ))
        ))
    ))
```

There's a single character typo in the code so that it uses `Nu` instead of
`Num`.  The resulting type error is impressive.

```console
$ ocamlc -c broken_poly.ml 
File "broken_poly.ml", line 11, characters 10-154:
Error: This expression has type
         [> `Add of
              ([< `Add of 'a * 'a
                | `Mul of 'a * 'a
                | `Num of int
                | `Sub of 'a * 'a
                > `Num ]
               as 'a) *
              [> `Sub of 'a * [> `Mul of [> `Nu of int ] * [> `Num of int ] ] ] ]
       but an expression was expected of type 'a
       The second variant type does not allow tag(s) `Nu
```

The type error is perfectly accurate, but rather verbose and with a line number
that doesn't point to the exact location of the incorrect variant name.  The
best the compiler can do is to point you in the general direction of the
`algebra` function application. 

This is because the type checker doesn't have enough information to match the
inferred type of the `algebra` definition to its application a few lines down.
It calculates types for both expressions separately, and when they don't match
up, outputs the difference as best it can.

Let's see what happens with an explicit type annotation to help the compiler
out.

```ocaml
(* broken_poly_with_annot.ml *)

type t = [
  | `Add of t * t
  | `Sub of t * t
  | `Mul of t * t
  | `Num of int
]

let rec algebra (x:t) =
  match x with
  | `Add (x,y) -> (algebra x) + (algebra y)
  | `Sub (x,y) -> (algebra x) - (algebra y)
  | `Mul (x,y) -> (algebra x) * (algebra y)
  | `Num x     -> x

let _ =
  algebra (
    `Add (
      (`Num 0),
      (`Sub (
          (`Num 1),
          (`Mul (
              (`Nu 3),(`Num 2)
            ))
        ))
    ))
```

This code contains exactly the same error as before, but we've added a closed
type definition of the polymorphic variants, and a type annotation to the
`algebra` definition.  The compiler error we get is much more useful now.

```console
$ ocamlc -i broken_poly_with_annot.ml 
File "broken_poly_with_annot.ml", line 24, characters 14-21:
Error: This expression has type [> `Nu of int ]
       but an expression was expected of type t
       The second variant type does not allow tag(s) `Nu
```

This error points directly to the correct line number that contains the typo.
Once you fix the problem, you can remove the manual annotations if you prefer
more succinct code.  You can also leave the annotations there of course, to
help with future refactoring and debugging.

#### Enforcing principal typing

The compiler also has a stricter *principal type checking* mode that is
activated via the `-principal` flag.  This warns about risky uses of type
information to ensure that the type inference has one principal result.  A type
is considered risky if the success or failure of type inference depends on the
order in which sub-expressions are typed.

The principality check only affects a few language features:

* polymorphic methods for objects.
* permuting the order of labeled arguments in a function from their type definition.
* discarding optional labelled arguments.
* generalized algebraic data types (GADTs) present from OCaml 4.0 onwards.
* automatic disambiguation of record field and constructor names (since OCaml 4.1)

Here's an example of principality warnings when used with record disambiguation.

```ocaml
(* non_principal.ml *)
type s = { foo: int; bar: unit }
type t = { foo: int }

let f x =
  x.bar;
  x.foo
```

Inferring the signature with `-principal` will show you a new warning.

```console
$ ocamlc -i -principal non_principal.ml 
File "non_principal.ml", line 7, characters 4-7:
Warning 18: this type-based field disambiguation is not principal.
type s = { foo : int; bar : unit; }
type t = { foo : int; }
val f : s -> int
```

This example isn't principal since the inferred type for `x.foo` is guided by
the inferred type of `x.bar`, whereas principal typing requires that each
sub-expression's type can be calculated independently.  If the `x.bar` use is
removed from the definition of `f`, its argument would be of type `t` and not
`type s`.

You can fix this either by permuting the order of the type declarations, or by
adding an explicit type annotation.

```ocaml
(* principal.ml *)
type s = { foo: int; bar: unit }
type t = { foo: int }

let f (x:s) =
  x.bar;
  x.foo
```

There is now no ambiguity about the inferred types, since we've explicitly
given the argument a type and the order of inference of the sub-expressions no
longer matters.

```console
$ ocamlc -i -principal principal.ml 
type s = { foo : int; bar : unit; }
type t = { foo : int; }
val f : s -> int
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

### Modules and separate compilation

The OCaml module system enables smaller components to be reused effectively in
large projects while still retaining all the benefits of static type safety.
We covered the basics of using modules earlier in
[xref](#files-modules-and-programs).  The module language that operates over
these signatures also extends to functors and first-class modules, described in
[xref](#functors) and [xref](#first-class-modules) respectively.

This section discusses how the compiler implements them in more detail.
Modules are essential for larger projects that consist of many source files
(also known as *compilation units*).   It's impractical to recompile every
single source file when changing just one or two files, and the module system
minimizes such recompilation while still encouraging code reuse.

#### The mapping between files and modules

Individual compilation units provide a convenient way to break up a big module
hierarchy into a collection of files.  The relationship between files and
modules can be explained directly in terms of the module system.

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

These two files are exactly analogous to including the following code directly
in another module that references `Alice`.

```ocaml
module Alice : sig
  val friends : Bob.t list
end = struct
  let friends = [ Bob.name ]
end
```

#### Defining a module search path

In the example above, `Alice` also has a reference to another module `Bob`.
For the overall type of `Alice` to be valid, the compiler also needs to check
that the `Bob` module contains at least a `Bob.name` value and defines a
`Bob.t` type.

The type checker resolves such module references into concrete structures and
signatures in order to unify types across module boundaries.  It does this by
searching a list of directories for a compiled interface file matching that
module's name.  For example, it will look for `alice.cmi` and `bob.cmi` on the
search path, and use the first ones it encounters as the interfaces for `Alice`
and `Bob`.

The module search path is set by adding `-I` flags to the compiler command-line
with the directory containing the `cmi` files as the argument.  Manually
specifying these flags gets complex when you have lots of libraries, and is the
reason why the OCamlfind frontend to the compiler exists.  OCamlfind automates
the process of turning third-party package names and build descriptions into
command-line flags that are passed to the compiler command-line.

By default, only the current directory and the OCaml standard library will be
searched for `cmi` files.  The `Pervasives` module from the standard library
will also be opened by default in every compilation unit.  The standard library
location is obtained by running `ocamlc -where`, and can be overridden by
setting the `CAMLLIB` environment variable.  Needless to say, don't override
the default path unless you have a good reason to (such as setting up a
cross-compilation environment).

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

### Shorter module paths in type errors

Core uses the OCaml module system quite extensively to provide a complete
replacement standard library.  It collects these modules into a single `Std`
module which provides a single module that needs to be opened to import
the replacement modules and functions.

There's one downside to this approach: type errors suddenly get much more
verbose.  We can see this if you run the vanilla OCaml top-level (not `utop`).

```console
$ ocaml
# List.map print_endline "" ;;
Error: This expression has type string but an expression was expected of type
         string list
```

This type error without `Core.Std` has a straightforward type error.  When
we switch to Core, though, it gets more verbose.

```console
# open Core.Std ;;
# List.map ~f:print_endline "" ;;
Error: This expression has type string but an expression was expected of type
         'a Core.Std.List.t = 'a list
```

The default `List` module in OCaml is overridden by `Core.Std.List`.  The
compiler does its best to show the type equivalence, but at the cost of
a more verbose error message.

The compiler can remedy this via a so-called "short paths" heuristic.  This
causes the compiler to search all the type aliases for the shortest module
path, and use that as the preferred output type.  The option is activated
by passing `-short-paths` to the compiler, and works on the top-level too.

```console
$ ocaml -short-paths
# open Core.Std;;
# List.map ~f:print_endline "foo";;
Error: This expression has type string but an expression was expected of type
         'a list
```

The `utop` enhanced top-level activates short paths by default, which is why
you've not had to do this before in our interactive examples.  However, the
compiler doesn't default to the short path heuristic since there are some
situations where the type aliasing information is useful to know, and would be
lost in the error if the shortest module path is always picked.

You'll need to choose for yourself if you prefer short paths or the default
behaviour in your own projects, and pass the `-short-paths` flag to the
compiler if you need it.

## The typed syntax tree

When the type checking process has successfully completed, it is combined with
the AST to form a *typed abstract syntax tree*.  This contains precise location
information for every token in the input file, and decorates each token with
concrete type information.

The compiler can output this as compiled `cmt` and `cmti` files that contain
the typed AST for the implementation and signatures of a compilation unit.
This is activated by passing the `-bin-annot` flag to the compiler.

The `cmt` files are particularly useful for IDE tools to match up OCaml source
code at a specific location to the inferred or external types.  

### Using ocp-index for auto-completion

One such command-line tool to display auto-completion information in your
editor is `ocp-index`.  Install it via OPAM as follows.

```console
$ opam install ocp-index
$ ocp-index
```

Let's refer back to our Ncurses binding example from the beginning of
[xref](#foreign-function-interface).  This module defined bindings for the
Ncurses library.  First, compile the interfaces with `-bin-annot` so that we
can obtain the `cmt` and `cmti` files.

```console
$ ocamlfind ocamlopt -bin-annot -c -package ctypes.foreign \
    ncurses.mli ncurses.ml
```

Next, run `ocp-index` in completion mode.  You pass it a set of directories to
search for `cmt` files in, and a fragment of text to autocomplete.

```console
$ ocp-index complete -I . Ncur
Ncurses module

$ ocp-index complete -I . Ncurses.a
Ncurses.addstr val string -> unit
Ncurses.addch val char -> unit

$ ocp-index complete -I . Ncurses.
Ncurses.cbreak val unit -> unit
Ncurses.box val Ncurses.window -> int -> int -> unit
Ncurses.mvwaddstr val Ncurses.window -> int -> int -> string -> unit
Ncurses.mvwaddch val Ncurses.window -> int -> int -> char -> unit
Ncurses.addstr val string -> unit
Ncurses.addch val char -> unit
Ncurses.newwin val int -> int -> int -> int -> Ncurses.window
Ncurses.refresh val unit -> unit
Ncurses.endwin val unit -> unit
Ncurses.initscr val unit -> Ncurses.window
Ncurses.wrefresh val Ncurses.window -> unit
Ncurses.window val Ncurses.window Ctypes.typ
```

As you can imagine, autocompletion is invaluable on larger codebases.  See the
[ocp-index](https://github.com/ocamlpro/ocp-index) homepage for more
information on how to integrate it with your favourite editor.

### Examining the typed syntax tree directly

The compiler has a couple of advanced flags that can dump the raw output of the
internal AST representation.  You can't depend on these flags to give the same
output across compiler revisions, but they are a useful learning tool.

We'll use our toy `typedef.ml` again.

```ocaml
(* typedef.ml *)
type t = Foo | Bar
let v = Foo
```

Let's first look at the untyped syntax tree that's generated from the parsing phase.

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

This is rather a lot of output for a simple two-line program, but it shows just
how much structure the OCaml parser generates even from a small source file.

Each portion of the AST is decorated with the precise location information
(including the filename and character location of the token).  This code hasn't
been type checked yet, and so the raw tokens are all included.

The typed AST that is normally output as a compiled `cmt` file can be displayed
in a more developer-readable form via the `-dtypedtree` option.

```console
$ ocamlc -dtypedtree typedef.ml
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

You'll rarely need to look at this raw output from the compiler unless you're
building IDE tools such as `ocp-index`, or are hacking on extensions to the
core compiler itself.  However, it's useful to know that this intermediate form
exists before we delve further into the code generation process next in
[xref](the-compiler-backend-byte-code-and-native-code).


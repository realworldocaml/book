# The Compiler Frontend: Parsing and <span class="keep-together">Type Checking</span> {#the-compiler-frontend-parsing-and-type-checking data-type=chapter}

Compiling source code into executable programs is a fairly complex libraries,
linkers, and assemblers. It's important to understand how these fit together
to help with your day-to-day workflow of developing, debugging, and deploying
applications.[compilation process/toolchain for]{.idx}

OCaml has a strong emphasis on static type safety and rejects source code
that doesn't meet its requirements as early as possible. The compiler does
this by running the source code through a series of checks and
transformations. Each stage performs its job (e.g., type checking,
optimization, or code generation) and discards some information from the
previous stage. The final native code output is low-level assembly code that
doesn't know anything about the OCaml modules or objects that the compiler
started with.[static checking]{.idx}[compile-time static checking]{.idx}

You don't have to do all of this manually, of course. The compiler frontends
(`ocamlc` and `ocamlopt`) are invoked via the command line and chain the
stages together for you. Sometimes though, you'll need to dive into the
toolchain to hunt down a bug or investigate a performance problem. This
chapter explains the compiler pipeline in more depth so you understand how to
harness the command-line tools effectively. [OCaml
toolchain/ocamlc]{.idx}[OCaml toolchain/ocamlopt]{.idx}

In this chapter, we'll cover the following topics:

- The compilation pipeline and what each stage represents

- Source preprocessing via Camlp4 and the intermediate forms

- The type-checking process, including module resolution

The details of the compilation process into executable code can be found
next, in
[The Compiler Backend Byte Code And Native Code](compiler-backend.html#the-compiler-backend-byte-code-and-native-code){data-type=xref}.

## An Overview of the Toolchain {#an-overview-of-the-toolchain data-type=sect1}

The OCaml tools accept textual source code as input, using the filename
extensions `.ml` and `.mli` for modules and signatures, respectively. We
explained the basics of the build process in
[Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref},
so we'll assume you've built a few OCaml programs already by this
point.[OCaml toolchain/overview of]{.idx}

Each source file represents a *compilation unit* that is built separately.
The compiler generates intermediate files with different filename extensions
to use as it advances through the compilation stages. The linker takes a
collection of compiled units and produces a standalone executable or library
archive that can be reused by other applications.[compilation units]{.idx}

The overall compilation pipeline looks like this: [compilation
process/diagram of]{.idx}

<figure style="float: 0">
  <img src="images/front-end/pipeline.png"/>
</figure>


Notice that the pipeline branches toward the end. OCaml has multiple compiler
backends that reuse the early stages of compilation but produce very
different final outputs. The *bytecode* can be run by a portable interpreter
and can even be transformed into JavaScript (via
[js_of_ocaml](http://ocsigen.org/js_of_ocaml)) or C source code (via
[OCamlCC](https://github.com/ocaml-bytes/ocamlcc)). The *native code*
compiler generates specialized executable binaries suitable for
high-performance applications.[compilation process/compiler source
code]{.idx}[code compilers/bytecode vs. native code]{.idx}

<aside data-type="sidebar">
<h5>Obtaining the Compiler Source Code</h5>

Although it's not necessary to understand the examples, you may find it
useful to have a copy of the OCaml source tree checked out while you read
through this chapter. The source code is available from multiple places:

- Stable releases as <em class="filename">zip</em> and
  <em class="filename">tar</em> archives from the
  [OCaml download site](http://caml.inria.fr/download.en.html)

- A Subversion anonymous mirror of the main development sources available on
  the [development resources](http://caml.inria.fr/ocaml/anonsvn.en.html)
  page online

- A Git mirror of the Subversion repository with all the history and
  development branches included, browsable online at
  [GitHub](https://github.com/ocaml/ocaml)

The source tree is split up into subdirectories. The core compiler consists
of:

`config/`
: Configuration directives to tailor OCaml for your operating system and
  architecture.

`bytecomp/` and `byterun/`
: Bytecode compiler and runtime, including the garbage collector (GC).

`asmcomp/` and `asmrun/`
: Native-code compiler and runtime. The native runtime symlinks many modules
  from the `byterun` directory to share code, most notably the GC.

`parsing/`
: The OCaml lexer, parser, and libraries for manipulating them.

`typing/`
: The static type checking implementation and type definitions.

`camlp4/`
: The source code macro preprocessor.

`driver/`
: Command-line interfaces for the compiler tools.

A number of tools and scripts are also built alongside the core compiler:

`debugger/`
: The interactive bytecode debugger.

`toplevel/`
: Interactive top-level console.

`emacs/`
: A *caml-mode* for the Emacs editor.

`stdlib/`
: The compiler standard library, including the `Pervasives` module.

`ocamlbuild/`
: Build system that automates common OCaml compilation modes.

`otherlibs/`
: Optional libraries such as the Unix and graphics modules.

`tools/`
: Command-line utilities such as `ocamldep` that are installed with the
  compiler.

`testsuite/`
: Regression tests for the core compiler.

</aside>

We'll go through each of the compilation stages now and explain how they will
be useful to you during day-to-day OCaml development.

## Parsing Source Code {#parsing-source-code data-type=sect1}

When a source file is passed to the OCaml compiler, its first task is to
parse the text into a more structured abstract syntax tree (AST). The parsing
logic is implemented in OCaml itself using the techniques described earlier
in
[Parsing With Ocamllex And Menhir](parsing-with-ocamllex-and-menhir.html#parsing-with-ocamllex-and-menhir){data-type=xref}.
The lexer and parser rules can be found in the `parsing` directory in the
source distribution.[AST (abstract syntax-tree)]{.idx}[source code/parsing
of]{.idx #SCpras}[parsing/of source code]{.idx #PARSsource}[compilation
process/parsing source code]{.idx #CPpars}

### Syntax Errors {#syntax-errors data-type=sect2}

The OCaml parser's goal is to output a well-formed AST data structure to the
next phase of compilation, and so it any source code that doesn't match basic
syntactic requirements. The compiler emits a *syntax error* in this
situation, with a pointer to the filename and line and character number
that's as close to the error as possible.[errors/syntax errors]{.idx}[syntax
errors]{.idx}

Here's an example syntax error that we obtain by performing a module
assignment as a statement instead of as a `let` binding:

<link rel="import" href="code/front-end/broken_module.ml" />

The code results in a syntax error when compiled:

<link rel="import" href="code/front-end/build_broken_module.errsh" />

The correct version of this source code creates the `MyString` module
correctly via a local open, and compiles successfully:

<link rel="import" href="code/front-end/fixed_module.ml" />

The syntax error points to the line and character number of the first token
that couldn't be parsed. In the broken example, the `module` keyword isn't a
valid token at that point in parsing, so the error location information is
correct.

### Automatically Indenting Source Code {#automatically-indenting-source-code data-type=sect2}

Sadly, syntax errors do get more inaccurate sometimes, depending on the
nature of your mistake. Try to spot the deliberate error in the following
function definitions: [source code/automatically indenting]{.idx}

<link rel="import" href="code/front-end/follow_on_function.ml" />

When you compile this file, you'll get a syntax error again:

<link rel="import" href="code/front-end/build_follow_on_function.errsh" />

The line number in the error points to the end of the `add_and_print`
function, but the actual error is at the end of the *first* function
definition. There's an extra semicolon at the end of the first definition
that causes the second definition to become part of the first `let` binding.
This eventually results in a parsing error at the very end of the second
function.

This class of bug (due to a single errant character) can be hard to spot in a
large body of code. Luckily, there's a great tool available via OPAM called
`ocp-indent` that applies structured indenting rules to your source code on a
line-by-line basis. This not only beautifies your code layout, but it also
makes this syntax error much easier to locate.[debugging/single errant
characters]{.idx}

Let's run our erroneous file through `ocp-indent` and see how it processes
it:

<link rel="import" href="code/front-end/indent_follow_on_function.sh" />

The `add_and_print` definition has been indented as if it were part of the
first `concat_and_print` definition, and the errant semicolon is now much
easier to spot. We just need to remove that semicolon and rerun `ocp-indent`
to verify that the syntax is correct:

<link rel="import" href="code/front-end/indent_follow_on_function_fixed.sh" />

The `ocp-indent`[home page](https://github.com/OCamlPro/ocp-indent) documents
how to integrate it with your favorite editor. All the Core libraries are
formatted using it to ensure consistency, and it's a good idea to do this
before publishing your own source code online.

### Generating Documentation from Interfaces {#generating-documentation-from-interfaces data-type=sect2}

Whitespace and source code comments are removed during parsing and aren't
significant in determining the semantics of the program. However, other tools
in the OCaml distribution can interpret comments for their own ends. [OCaml
toolchain/ocamldoc]{.idx}[interfaces/generating documentation
from]{.idx}[documentation, generating from interfaces]{.idx}

The `ocamldoc` tool uses specially formatted comments in the source code to
generate documentation bundles. These comments are combined with the function
definitions and signatures, and output as structured documentation in a
variety of formats. It can generate HTML pages, LaTeX and PDF documents, UNIX
manual pages, and even module dependency graphs that can be viewed using
[Graphviz](http://www.graphviz.org).

Here's a sample of some source code that's been annotated with `ocamldoc`
comments:

<link rel="import" href="code/front-end/doc.ml" />

The `ocamldoc` comments are distinguished by beginning with the double
asterisk. There are formatting conventions for the contents of the comment to
mark metadata. For instance, the `@tag` fields mark specific properties such
as the author of that section of code.

Try compiling the HTML documentation and UNIX man pages by running `ocamldoc`
over the source file:

<link rel="import" href="code/front-end/build_ocamldoc.rawsh" />

You should now have HTML files inside the <em class="filename">html/</em>
directory and also be able to view the UNIX manual pages held in
<em class="filename">man/man3</em>. There are quite a few comment formats and
options to control the output for the various backends. Refer to the
[ OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html)
for the complete list.[Xen]{.idx}[JSON data/Xen custom generator
for]{.idx}[Bibtex]{.idx}[OCaml toolchain/ocamldoc-generators]{.idx}[Argot
HTML generator]{.idx}[HTML
generators]{.idx}<a data-type="indexterm" data-startref="SCpras">&nbsp;</a><a data-type="indexterm" data-startref="PARSsource">&nbsp;</a><a data-type="indexterm" data-startref="CPpars">&nbsp;</a>

::: {data-type=tip}
#### Using Custom ocamldoc Generators

The default HTML output stylesheets from `ocamldoc` are pretty spartan and
distinctly Web 1.0. The tool supports plugging in custom documentation
generators, and there are several available that provide prettier or more
detailed output:

- [Argot](http://argot.x9c.fr/) is an enhanced HTML generator that supports
  code folding and searching by name or type definition.

- [ ocamldoc generators](https://gitorious.org/ocamldoc-generators/ocamldoc-generators)
  add support for Bibtex references within comments and generating literate
  documentation that embeds the code alongside the comments.

- JSON output is available via a custom
  [generator](https://github.com/xen-org/ocamldoc-json) in Xen.
:::



## Preprocessing Source Code {#preprocessing-source-code data-type=sect1}

One powerful feature in OCaml is a facility to extend the standard-language
grammar without having to modify the compiler. You can roughly think of it as
a type-safe version of the `cpp` preprocessor used in C/C++ to control
conditional compilation directives.[grammars/extension of standard
language]{.idx}[source code/preprocessing of]{.idx #SCpreproc}[compilation
process/preprocessing source code]{.idx #CPpreproc}

The OCaml distribution includes a system called Camlp4 for writing extensible
parsers. This provides some OCaml libraries that are used to define grammars,
as well as dynamically loadable syntax extensions of such grammars. Camlp4
modules register new language keywords and later transform these keywords (or
indeed, any portion of the input program) into conventional OCaml code that
can be understood by the rest of the compiler.[syntax extension/in
Camlp4]{.idx #SEcamlp}[programming/dynamic programming]{.idx}[dynamic
programming]{.idx}[Bin_prot library]{.idx}[Sexplib package/sexp
converter]{.idx}[fieldslib]{.idx}[parsing/extensible
parsers]{.idx}[extensible parsers]{.idx}[Camlp4 syntax extension
mechanism]{.idx #camlp}

We've already seen several Core libraries that use Camlp4:

`Fieldslib`
: Generates first-class values that represent fields of a record

`Sexplib`
: To convert types to textual s-expressions

`Bin_prot`
: For efficient binary conversion and parsing

These libraries all extend the language in quite a minimal way by adding a
`with` keyword to type declarations to signify that extra code should be
generated from that declaration. For example, here's a trivial use of Sexplib
and Fieldslib:

<link rel="import" href="code/front-end/type_conv_example.ml" />

Compiling this code will normally give you a syntax error if you do so
without Camlp4, since the `with` keyword isn't normally allowed after a type
definition:

<link rel="import" href="code/front-end/build_type_conv_without_camlp4.errsh" />

Now add in the syntax extension packages for Fieldslib and Sexplib, and
everything will compile again:

<link rel="import" href="code/front-end/build_type_conv_with_camlp4.rawsh" />

We've specified a couple of additional flags here. The `-syntax` flag directs
`ocamlfind` to add the `-pp` flag to the compiler command line. This flag
instructs the compiler to run the preprocessor during its parsing phase.

The `-package` flag imports other OCaml libraries. The `.syntax` suffix in
the package name is a convention that indicates these libraries are
preprocessors that should be run during parsing. The syntax extension modules
are dynamically loaded into the `camlp4o` command, which rewrites the input
source code into conventional OCaml code that has no trace of the new
keywords. The compiler then compiles this transformed code with no knowledge
of the preprocessor's actions.

Both Fieldslib and Sexplib need this new `with` keyword, but they both can't
register the same extension. Instead, a library called Type_conv provides the
common extension framework for them to use. Type_conv registers the `with`
grammar extension to Camlp4, and the OCamlfind packaging ensures that it's
loaded before Fieldslib or Sexplib.

The two extensions generate boilerplate OCaml code based on the type
definition at compilation time. This avoids the performance hit of doing the
code generation dynamically and also doesn't require a just-in-time (JIT)
runtime that can be a source of unpredictable dynamic behavior. Instead, all
the extra code is simply generated at compilation time via Camlp4, and type
information can be discarded from the runtime image. ["Just-in-Time" dynamic
patching]{.idx data-primary-sortas=Just}

The syntax extensions accept an input AST and output a modified one. If
you're not familiar with the Camlp4 module in question, how do you figure out
what changes it's made to your code? The obvious way is to read the
documentation that accompanies the extension. Another approach is to use the
toplevel to explore the extension's behavior or run Camlp4 manually yourself
to see the transformation in action. We'll show you how to do both of these
now.

### Using Camlp4 Interactively {#using-camlp4-interactively data-type=sect2}

The `utop` toplevel can run the phrases that you type through `camlp4`
automatically. You should have at least these lines in your `~/.ocamlinit`
file in your home directory (see
[this Real World OCaml page](http://realworldocaml.org/install) for more
information):

<link rel="import" href="code/front-end/camlp4_toplevel.mlt" part="0.5" />

The first directive loads the `ocamlfind` top-level interface that lets you
require `ocamlfind` packages (including all their dependent packages). The
second directive instructs the toplevel to filter all phrases via Camlp4. You
can now run `utop` and load the syntax extensions in. We'll use the
`comparelib` syntax extension for our experiments.

OCaml provides a built-in polymorphic comparison operator that inspects the
runtime representation of two values to see if they're equal. As we noted in
[Maps And Hash Tables](maps-and-hashtables.html#maps-and-hash-tables){data-type=xref},
the polymorphic comparison is less efficient than defining explicit
comparison functions between values. However, it quickly becomes tedious to
manually define comparison functions for complex type definitions.
[interactive input/with camlp4]{.idx}[polymorphic comparisons]{.idx}

Let's see how `comparelib` solves this problem by running it in `utop`:

<link rel="import" href="code/front-end/camlp4_toplevel.mlt" part="1" />

The first definition of `t` is a standard OCaml phrase and results in the
expected output. The second one includes the `with compare` directive. This
is intercepted by `comparelib` and transformed into the original type
definition with two new functions also
<span class="keep-together">included</span>.

### Running Camlp4 from the Command Line {#running-camlp4-from-the-command-line data-type=sect2}

The toplevel is a quick way to examine the signatures generated from the
extensions, but how can we see what these new functions actually do? We can't
do this from `utop` directly, since it embeds the Camlp4 invocation as an
automated part of its operation.[command-line parsing/with Camlp4]{.idx}

Let's turn to the command line to obtain the result of the `comparelib`
transformation instead. Create a file that contains the type declaration from
earlier:

<link rel="import" href="code/front-end/comparelib_test.ml" />

We need to run the Camlp4 binary with the library paths to Comparelib and
Type_conv. Let's use a small shell script to wrap this invocation:

<link rel="import" href="code/front-end/camlp4_dump.cmd" />

The script uses the `ocamlfind` package manager to list the include and
library paths needed by `comparelib`. It then invokes the `camlp4o`
preprocessor with these paths and outputs the resulting AST to the standard
output:

<link rel="import" href="code/front-end/process_comparelib_test.sh" />

The output contains the original type definition accompanied by some
automatically generated code that implements an explicit comparison function
for each field in the record. If you're using the extension in your compiler
command line, this generated code is then compiled as if you had typed it in
yourself.

Note that although the generated code uses `Pervasives.compare`, it is also
annotated with a `string` type. This lets the compiler use a specialized
string comparison function and not actually call the runtime polymorphic
comparison function. This has implications for correctness, too: recall from
[Maps And Hash Tables](maps-and-hashtables.html#maps-and-hash-tables){data-type=xref}
that `comparelib` provides reliable comparison functions that work for values
that are logically the same but that have differing internal representations
(e.g., `Int.Set.t`).[wildcards]{.idx}[bindings/wildcards in let
bindings]{.idx}[let syntax/wildcards in bindings]{.idx}

::: {.allow_break data-type=note}
#### A Style Note: Wildcards in let Bindings

You may have noticed the `let _ = fun` construct in the autogenerated code
above. The underscore in a `let` binding is just the same as a wildcard
underscore in a pattern match, and tells the compiler to accept any return
value and discard it immediately.

This is fine for mechanically generated code from Type_conv but should be
avoided in code that you write by hand. If it's a unit-returning expression,
then write a `unit` binding explicitly instead. This will cause a type error
if the expression changes type in the future (e.g., due to code refactoring):
:::


<link rel="import" href="code/front-end/let_unit.syntax" />

If the expression has a different type, then write it explicitly:

<link rel="import" href="code/front-end/let_notunit.ml" />

The last one is used to ignore Async expressions that should run in the
background rather than blocking in the current thread.

One other important reason for using wildcard matches is to bind a variable
name to something that you want to use in future code but don't want to use
right away. This would normally generate an "unused value" compiler warning.
These warnings are suppressed for any variable name that's prepended with an
underscore:

<link rel="import" href="code/front-end/unused_var.ml" />

Although you don't use `_z` in your code, this will never generate an unused
variable warning.

### Preprocessing Module Signatures {#preprocessing-module-signatures data-type=sect2}

Another useful feature of `type_conv` is that it can generate module
signatures, too. Copy the earlier type definition into a
`comparelib_test.mli` that's got exactly the same
<span class="keep-together">content</span>:[signatures/preprocessing module
signatures]{.idx}[modules/preprocessing signatures of]{.idx}

<link rel="import" href="code/front-end/comparelib_test.mli" />

If you rerun the Camlp4 dumper script now, you'll see that different code is
produced for signature files:

<link rel="import" href="code/front-end/process_comparelib_interface.sh" />

The external signature generated by `comparelib` is much simpler than the
actual code. Running Camlp4 directly on the original source code lets you see
these all these transformations precisely. [grammars/avoiding grammar
clashes]{.idx}[macros]{.idx}[conditional
compilation]{.idx}[whitespace-sensitive indentation]{.idx}[syntax
extension/potential overuse of]{.idx}

::: {.allow_break data-type=caution}
#### Don't Overdo the Syntax Extensions

Syntax extensions are a powerful extension mechanism that can completely
alter your source code's layout and style. Core includes a very conservative
set of extensions that take care to minimize the syntax changes. There are a
number of third-party libraries that are much more ambitious—some introduce
whitespace-sensitive indentation, while others build entirely new embedded
languages using OCaml as a host language, and yet others introduce
conditional compilation for macros or optional logging.

While it's tempting to compress all your boilerplate code into Camlp4
extensions, it can make your source code much harder for other people to
quickly read and understand. Core mainly focuses on type-driven code
generation using the `type_conv` extension and doesn't fundamentally change
the OCaml syntax.

Another thing to consider before deploying your own syntax extension is
compatibility with other extensions. Two separate extensions can create a
grammar clash that leads to odd syntax errors and hard-to-reproduce bugs.
That's why most of Core's syntax extensions go through `type_conv`, which
acts as a single point for extending the grammar via the `with` keyword.
:::


### Further Reading on Camlp4 {#further-reading-on-camlp4 data-type=sect2}

We've deliberately only shown you how to use Camlp4 extensions here, and not
how to build your own. The full details of building new extensions are fairly
daunting and could be the subject of an entirely new book.[syntax
extension/building new]{.idx}[extensions]{.idx}

The best resources to get started
are:<a data-type="indexterm" data-startref="SEcamlp">&nbsp;</a><a data-type="indexterm" data-startref="camlp">&nbsp;</a><a data-type="indexterm" data-startref="SCpreproc">&nbsp;</a><a data-type="indexterm" data-startref="CPpreproc">&nbsp;</a>

- A series of
  [ blog posts](http://ambassadortothecomputers.blogspot.co.uk/p/reading-camlp4.html)
  by Jake Donham describe the internals of Camlp4 and its syntax extension
  mechanism

- The online [Camlp4 wiki](http://brion.inria.fr/gallium/index.php/Camlp4)

- Using OPAM to install existing Camlp4 extensions and inspecting their
  source code


## Static Type Checking {#static-type-checking data-type=sect1}

After obtaining a valid abstract syntax tree, the compiler has to verify that
the code obeys the rules of the OCaml type system. Code that is syntactically
correct but misuses values is rejected with an explanation of the problem.

Although type checking is done in a single pass in OCaml, it actually
consists of three distinct steps that happen simultaneously:[explicit
subtyping]{.idx}[automatic type inference]{.idx}[subtyping/in static type
checking]{.idx}[modules/in static type checking]{.idx}[type inference/in
static type checking]{.idx}[compilation process/static type
checking]{.idx #CPstatictype}

automatic type inference
: An algorithm that calculates types for a module without requiring manual
  type annotations

module system
: Combines software components with explicit knowledge of their type
  signatures

explicit subtyping
: Checks for objects and polymorphic variants

Automatic type inference lets you write succinct code for a particular task
and have the compiler ensure that your use of variables is locally
consistent.

Type inference doesn't scale to very large codebases that depend on separate
compilation of files. A small change in one module may ripple through
thousands of other files and libraries and require all of them to be
recompiled. The module system solves this by providing the facility to
combine and manipulate explicit type signatures for modules within a large
project, and also to reuse them via functors and first-class
modules.[modules/benefits of]{.idx}[type inference/drawbacks of]{.idx}

Subtyping in OCaml objects is always an explicit operation (via the `:>`
operator). This means that it doesn't complicate the core type inference
engine and can be tested as a separate concern.

### Displaying Inferred Types from the Compiler {#displaying-inferred-types-from-the-compiler data-type=sect2}

We've already seen how you can explore type inference directly from the
toplevel. It's also possible to generate type signatures for an entire file
by asking the compiler to do the work for you. Create a file with a single
type definition and value:

<link rel="import" href="code/front-end/typedef.ml" />

Now run the compiler with the `-i` flag to infer the type signature for that
file. This runs the type checker but doesn't compile the code any further
after displaying the interface to the standard output:

<link rel="import" href="code/front-end/infer_typedef.sh" />

The output is the default signature for the module that represents the input
file. It's often useful to redirect this output to an `mli` file to give you
a starting signature to edit the external interface without having to type it
all in by hand.

The compiler stores a compiled version of the interface as a `cmi` file. This
interface is either obtained from compiling an `mli` signature file for a
module, or by the inferred type if there is only an `ml` implementation
present.

The compiler makes sure that your `ml` and `mli` files have compatible
signatures. The type checker throws an immediate error if this isn't the
case:

<link rel="import" href="code/front-end/conflicting_interface.ml" />

<link rel="import" href="code/front-end/conflicting_interface.mli" />

<link rel="import" href="code/front-end/conflicting_interfaces.errsh" />

::: {.allow_break data-type=note}
#### Which Comes First: The ml or the mli?

There are two schools of thought on which order OCaml code should be written
in. It's very easy to begin writing code by starting with an `ml` file and
using the type inference to guide you as you build up your functions. The
`mli` file can then be generated as described, and the exported functions
documented.[code compilers/order of code]{.idx}[mli files]{.idx}[files/mli
files]{.idx}[ml files]{.idx}[files/ml files]{.idx}

If you're writing code that spans multiple files, it's sometimes easier to
start by writing all the `mli` signatures and checking that they type-check
against one another. Once the signatures are in place, you can write the
implementations with the confidence that they'll all glue together correctly,
with no cyclic dependencies among the modules.

As with any such stylistic debate, you should experiment with which system
works best for you. Everyone agrees on one thing though: no matter in what
order you write them, production code should always explicitly define an
`mli` file for every `ml` file in the project. It's also perfectly fine to
have an `mli` file without a corresponding `ml` file if you're only declaring
signatures (such as module types).

Signature files provide a place to write succinct documentation and to
abstract internal details that shouldn't be exported. Maintaining separate
signature files also speeds up incremental compilation in larger code bases,
since recompiling a `mli` signature is much faster than a full compilation of
the implementation to native code.
:::


### Type Inference {#type-inference-1 data-type=sect2}

Type inference is the process of determining the appropriate types for
expressions based on their use. It's a feature that's partially present in
many other languages such as Haskell and Scala, but OCaml embeds it as a
fundamental feature throughout the core language. [Hindley-Milner
algorithm]{.idx}[type inference/algorithm basis of]{.idx}

OCaml type inference is based on the Hindley-Milner algorithm, which is
notable for its ability to infer the most general type for an expression
without requiring any explicit type annotations. The algorithm can deduce
multiple types for an expression and has the notion of a *principal type*
that is the most general choice from the possible inferences. Manual type
annotations can specialize the type explicitly, but the automatic inference
selects the most general type unless told otherwise.

OCaml does have some language extensions that strain the limits of principal
type inference, but by and large, most programs you write will never
*require* annotations (although they sometimes help the compiler produce
better error messages).

#### Adding type annotations to find errors {#adding-type-annotations-to-find-errors data-type=sect3}

It's often said that the hardest part of writing OCaml code is getting past
the type checker—but once the code does compile, it works correctly the
first time! This is an exaggeration of course, but it can certainly feel true
when moving from a dynamically typed language. The OCaml static type system
protects you from certain classes of bugs such as memory errors and
abstraction violations by rejecting your program at compilation time rather
than by generating an error at runtime. Learning how to navigate the type
checker's compile-time feedback is key to building robust libraries and
applications that take full advantage of these static checks.[type
inference/error detection with]{.idx}[annotations, for type
checking]{.idx}[errors/detecting with type annotations]{.idx}[type
annotations]{.idx}[compile-time static checking]{.idx}

There are a couple of tricks to make it easier to quickly locate type errors
in your code. The first is to introduce manual type annotations to narrow
down the source of your error more accurately. These annotations shouldn't
actually change your types and can be removed once your code is correct.
However, they act as anchors to locate errors while you're still writing your
code.

Manual type annotations are particularly useful if you use lots of
polymorphic variants or objects. Type inference with row polymorphism can
generate some very large signatures, and errors tend to propagate more widely
than if you are using more explicitly typed variants or classes.[polymorphic
variant types/type checking and]{.idx}[row polymorphism]{.idx}

For instance, consider this broken example that expresses some simple
algebraic operations over integers:

<link rel="import" href="code/front-end/broken_poly.ml" />

There's a single character typo in the code so that it uses `Nu` instead of
`Num`. The resulting type error is impressive:

<link rel="import" href="code/front-end/build_broken_poly.errsh" />

The type error is perfectly accurate, but rather verbose and with a line
number that doesn't point to the exact location of the incorrect variant
name. The best the compiler can do is to point you in the general direction
of the `algebra` function application.

This is because the type checker doesn't have enough information to match the
inferred type of the `algebra` definition to its application a few lines
down. It calculates types for both expressions separately, and when they
don't match up, outputs the difference as best it can.

Let's see what happens with an explicit type annotation to help the compiler
out:

<link rel="import" href="code/front-end/broken_poly_with_annot.ml" />

This code contains exactly the same error as before, but we've added a closed
type definition of the polymorphic variants, and a type annotation to the
`algebra` definition. The compiler error we get is much more useful now:

<link rel="import" href="code/front-end/build_broken_poly_with_annot.errsh" />

This error points directly to the correct line number that contains the typo.
Once you fix the problem, you can remove the manual annotations if you prefer
more succinct code. You can also leave the annotations there, of course, to
help with future refactoring and debugging.

#### Enforcing principal typing {#enforcing-principal-typing data-type=sect3}

The compiler also has a stricter *principal type checking* mode that is
activated via the <span class="keep-together">-principal</span> flag. This
warns about risky uses of type information to ensure that the type inference
has one principal result. A type is considered risky if the success or
failure of type inference depends on the order in which subexpressions are
typed.[type inference/principality checks]{.idx}[risky type]{.idx}[principal
type checking]{.idx}

The principality check only affects a few language features:

- Polymorphic methods for objects

- Permuting the order of labeled arguments in a function from their type
  definition

- Discarding optional labeled arguments

- Generalized algebraic data types (GADTs) present from OCaml 4.0 onward

- Automatic disambiguation of record field and constructor names (since OCaml
  4.1)

Here's an example of principality warnings when used with record
disambiguation.

<link rel="import" href="code/front-end/non_principal.ml" />

Inferring the signature with `-principal` will show you a new warning:

<link rel="import" href="code/front-end/build_non_principal.sh" />

This example isn't principal, since the inferred type for `x.foo` is guided
by the inferred type of `x.bar`, whereas principal typing requires that each
subexpression's type can be calculated independently. If the `x.bar` use is
removed from the definition of `f`, its argument would be of type `t` and not
`type s`.

You can fix this either by permuting the order of the type declarations, or
by adding an explicit type annotation:

<link rel="import" href="code/front-end/principal.ml" />

There is now no ambiguity about the inferred types, since we've explicitly
given the argument a type, and the order of inference of the subexpressions
no longer matters.

<link rel="import" href="code/front-end/build_principal.sh" />

The `ocamlbuild` equivalent is to add the tag `principal` to your build. The
*corebuild* wrapper script actually adds this by default, but it does no harm
to explicitly repeat it:

<link rel="import" href="code/front-end/build_principal_corebuild.sh" />

Ideally, all code should systematically use `-principal`. It reduces variance
in type inference and enforces the notion of a single known type. However,
there are drawbacks to this mode: type inference is slower, and the `cmi`
files become larger. This is generally only a problem if you extensively use
objects, which usually have larger type signatures to cover all their
methods.

If compiling in principal mode works, it is guaranteed that the program will
pass type checking in nonprincipal mode, too. For this reason, the
`corebuild` wrapper script activates principal mode by default, preferring
stricter type inference over a small loss in compilation speed and extra disk
space usage.

Bear in mind that the `cmi` files generated in principal mode differ from the
default mode. Try to ensure that you compile your whole project with it
activated. Getting the files mixed up won't let you violate type safety, but
it can result in the type checker failing unexpectedly very occasionally. In
this case, just recompile with a clean source tree.


### Modules and Separate Compilation {#modules-and-separate-compilation data-type=sect2}

The OCaml module system enables smaller components to be reused effectively
in large projects while still retaining all the benefits of static type
safety. We covered the basics of using modules earlier in
[Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref}.
The module language that operates over these signatures also extends to
functors and first-class modules, described in
[Functors](functors.html#functors){data-type=xref} and
[First Class Modules](first-class-modules.html#first-class-modules){data-type=xref},
respectively. [modules/separate compilation in]{.idx}

This section discusses how the compiler implements them in more detail.
Modules are essential for larger projects that consist of many source files
(also known as *compilation units*). It's impractical to recompile every
single source file when changing just one or two files, and the module system
minimizes such recompilation while still encouraging code reuse. [compilation
units]{.idx}

#### The mapping between files and modules {#the-mapping-between-files-and-modules data-type=sect3}

Individual compilation units provide a convenient way to break up a big
module hierarchy into a collection of files. The relationship between files
and modules can be explained directly in terms of the module system.
[files/relationship with modules]{.idx}

Create a file called `alice.ml` with the following contents:

<link rel="import" href="code/front-end/alice.ml" />

and a corresponding signature file:

<link rel="import" href="code/front-end/alice.mli" />

These two files are exactly analogous to including the following code
directly in another module that references `Alice`:

<link rel="import" href="code/front-end/alice_combined.ml" />

#### Defining a module search path {#defining-a-module-search-path data-type=sect3}

In the preceding example, `Alice` also has a reference to another module
`Bob`. For the overall type of `Alice` to be valid, the compiler also needs
to check that the `Bob` module contains at least a `Bob.name` value and
defines a `Bob.t` type. [modules/defining search paths]{.idx}

The type checker resolves such module references into concrete structures and
signatures in order to unify types across module boundaries. It does this by
searching a list of directories for a compiled interface file matching that
module's name. For example, it will look for `alice.cmi` and `bob.cmi` on the
search path and use the first ones it encounters as the interfaces for
`Alice` and `Bob`.

The module search path is set by adding `-I` flags to the compiler command
line with the directory containing the `cmi` files as the argument. Manually
specifying these flags gets complex when you have lots of libraries, and is
the reason why the OCamlfind frontend to the compiler exists. OCamlfind
automates the process of turning third-party package names and build
descriptions into command-line flags that are passed to the compiler command
line.

By default, only the current directory and the OCaml standard library will be
searched for `cmi` files. The `Pervasives` module from the standard library
will also be opened by default in every compilation unit. The standard
library location is obtained by running `ocamlc -where` and can be overridden
by setting the `CAMLLIB` environment variable. Needless to say, don't
override the default path unless you have a good reason to (such as setting
up a cross-compilation environment). [cmi files]{.idx}[files/cmi
files]{.idx}[OCaml toolchain/ocamlogjinfo]{.idx}

<aside data-type="sidebar">
<h5>Inspecting Compilation Units with ocamlobjinfo</h5>

For separate compilation to be sound, we need to ensure that all the 
`cmi` files used to type-check a module are the same across compilation runs.
If they vary, this raises the possibility of two modules checking different
type signatures for a common module with the same name. This in turn lets the
program completely violate the static type system and can lead to memory
corruption and crashes.

OCaml guards against this by recording a MD5 checksum in every `cmi`. Let's
examine our earlier `typedef.ml` more closely:

<link rel="import" href="code/front-end/typedef_objinfo.sh" />

`ocamlobjinfo` examines the compiled interface and displays what other
compilation units it depends on. In this case, we don't use any external
modules other than `Pervasives`. Every module depends on `Pervasives` by
default, unless you use the `-nopervasives` flag (this is an advanced use
case, and you shouldn't normally need it).

The long alphanumeric identifier beside each module name is a hash calculated
from all the types and values exported from that compilation unit. It's used
during type-checking and linking to ensure that all of the compilation units
have been compiled consistently against one another. A difference in the
hashes means that a compilation unit with the same module name may have
conflicting type signatures in different modules. The compiler will reject
such programs with an error similar to this:

<link rel="import" href="code/front-end/inconsistent_compilation_units.rawsh" />

This hash check is very conservative, but ensures that separate compilation
remains type-safe all the way up to the final link phase. Your build system
should ensure that you never see the preceding error messages, but if you do
run into it, just clean out your intermediate files and recompile from
scratch.

</aside>


### Packing Modules Together {#packing-modules-together data-type=sect2}

The module-to-file mapping described so far rigidly enforces a 1:1 mapping
between a top-level module and a file. It's often convenient to split larger
modules into separate files to make editing easier, but still compile them
all into a single OCaml module. [modules/packing together]{.idx}

The `-pack` compiler option accepts a list of compiled object files (
`.cmo` in bytecode and `.cmx` for native code) and their associated `.cmi`
compiled interfaces, and combines them into a single module that contains
them as submodules of the output. Packing thus generates an entirely new
`.cmo` (or `.cmx` file) and `.cmi` that includes the input modules.

Packing for native code introduces an additional requirement: the modules
that are intended to be packed must be compiled with the `-for-pack` argument
that specifies the eventual name of the pack. The easiest way to handle
packing is to let `ocamlbuild` figure out the command-line arguments for you,
so let's try that out next with a simple example.

First, create a couple of toy modules called `A.ml` and `B.ml` that contain a
single value. You will also need a `_tags` file that adds the `-for-pack`
option for the `cmx` files (but careful to exclude the pack target itself).
Finally, the `X.mlpack` file contains the list of modules that are intended
to be packed under module `X`. There are special rules in `ocamlbuild` that
tell it how to map `%.mlpack` files to the packed `%.cmx` or `%.cmo`
equivalent:

<link rel="import" href="code/packing/show_files.sh" />

You can now run *corebuild* to build the `X.cmx` file directly, but let's
create a new module to link against `X` to complete the example:

<link rel="import" href="code/packing/test.ml" />

You can now compile this test module and see that its inferred interface is
the result of using the packed contents of `X`. We further verify this by
examining the imported interfaces in `Test` and confirming that neither 
`A` nor `B` are mentioned in there and that only the packed `X` module is
used:

<link rel="import" href="code/packing/build_test.sh" />

::: {data-type=warning}
#### Packing and Search Paths

One very common build error that happens with packing is confusion resulting
from building the packed `cmi` in the same directory as the submodules. When
you add this directory to your module search path, the submodules are also
visible. If you forget to include the top-level prefix (e.g., `X.A`) and
instead use a submodule directly (`A`), then this will compile and link fine.

However, the types of `A` and `X.A` are *not* automatically equivalent so the
type checker will complain if you attempt to mix and match the packed and
unpacked versions of the library.

This mostly only happens with unit tests, since they are built at the same
time as the library. You can avoid it by being aware of the need to open the
packed module from the test, or only using the library after it has been
installed (and hence not exposing the intermediate compiled modules).
:::


### Shorter Module Paths in Type Errors {#shorter-module-paths-in-type-errors data-type=sect2}

Core uses the OCaml module system quite extensively to provide a complete
replacement standard library. It collects these modules into a single 
`Std` module, which provides a single module that needs to be opened to
import the replacement modules and functions. [errors/reducing verbosity
in]{.idx}

There's one downside to this approach: type errors suddenly get much more
verbose. We can see this if you run the vanilla OCaml toplevel (not `utop`).

<link rel="import" href="code/front-end/short_paths_1.rawsh" />

This type error without `Core` has a straightforward type error. When we
switch to Core, though, it gets more verbose:

<link rel="import" href="code/front-end/short_paths_2.rawsh" />

The default `List` module in OCaml is overridden by `Core.List`. The compiler
does its best to show the type equivalence, but at the cost of a more verbose
error message.

The compiler can remedy this via a so-called short paths heuristic. This
causes the compiler to search all the type aliases for the shortest module
path and use that as the preferred output type. The option is activated by
passing `-short-paths` to the compiler, and works on the toplevel, too.[short
paths heuristic]{.idx}

<link rel="import" href="code/front-end/short_paths_3.rawsh" />

The `utop` enhanced toplevel activates short paths by default, which is why
we have not had to do this before in our interactive examples. However, the
compiler doesn't default to the short path heuristic, since there are some
situations where the type aliasing information is useful to know, and it
would be lost in the error if the shortest module path is always picked.

You'll need to choose for yourself if you prefer short paths or the default
behavior in your own projects, and pass the `-short-paths` flag to the
compiler if you need
it.<a data-type="indexterm" data-startref="CPstatictype">&nbsp;</a>


## The Typed Syntax Tree {#the-typed-syntax-tree data-type=sect1}

When the type checking process has successfully completed, it is combined
with the AST to form a *typed abstract syntax tree*. This contains precise
location information for every token in the input file, and decorates each
token with concrete type information.[cmti files]{.idx}[cmt
files]{.idx}[files/cmtii files]{.idx}[files/cmt files]{.idx}[AST (abstract
syntax-tree)]{.idx}[typed syntax tree]{.idx #typesyntree}[compilation
process/typed syntax tree]{.idx #CPtypsyn}

The compiler can output this as compiled `cmt` and `cmti` files that contain
the typed AST for the implementation and signatures of a compilation unit.
This is activated by passing the `-bin-annot` flag to the compiler.

The `cmt` files are particularly useful for IDE tools to match up OCaml
source code at a specific location to the inferred or external types.

### Using ocp-index for Autocompletion {#using-ocp-index-for-auto-completion data-type=sect2}

One such command-line tool to display autocompletion information in your
editor is `ocp-index`. Install it via OPAM as
follows:[autocompletion]{.idx}[ocp-index]{.idx}

<link rel="import" href="code/front-end/install_ocp_index.rawsh" />

Let's refer back to our Ncurses binding example from the beginning of
[Foreign Function Interface](foreign-function-interface.html#foreign-function-interface){data-type=xref}.
This module defined bindings for the Ncurses library. First, compile the
interfaces with `-bin-annot` so that we can obtain the `cmt` and `cmti`
files, and then run `ocp-index` in completion mode:

<link rel="import" href="code/ocp-index/index_ncurses.sh" />

You need to pass `ocp-index` a set of directories to search for `cmt` files
in, and a fragment of text to autocomplete. As you can imagine,
autocompletion is invaluable on larger codebases. See the
[*ocp-index*](https://github.com/ocamlpro/ocp-index) home page for more
information on how to integrate it with your favorite editor.

### Examining the Typed Syntax Tree Directly {#examining-the-typed-syntax-tree-directly data-type=sect2}

The compiler has a couple of advanced flags that can dump the raw output of
the internal AST representation. You can't depend on these flags to give the
same output across compiler revisions, but they are a useful learning
tool.[flags]{.idx}

We'll use our toy `typedef.ml` again:

<link rel="import" href="code/front-end/typedef.ml" />

Let's first look at the untyped syntax tree that's generated from the parsing
phase:

<link rel="import" href="code/front-end/parsetree_typedef.sh" />

This is rather a lot of output for a simple two-line program, but it shows
just how much structure the OCaml parser generates even from a small source
file.

Each portion of the AST is decorated with the precise location information
(including the filename and character location of the token). This code
hasn't been type checked yet, so the raw tokens are all included.

The typed AST that is normally output as a compiled `cmt` file can be
displayed in a more developer-readable form via the `-dtypedtree` option:

<link rel="import" href="code/front-end/typedtree_typedef.sh" />

The typed AST is more explicit than the untyped syntax tree. For instance,
the type declaration has been given a unique name (`t/1008`), as has the 
`v` value (`v/1011`).
<a data-type="indexterm" data-startref="typesyntree">&nbsp;</a><a data-type="indexterm" data-startref="CPtypsyn">&nbsp;</a>

You'll rarely need to look at this raw output from the compiler unless you're
building IDE tools such as `ocp-index`, or are hacking on extensions to the
core compiler itself. However, it's useful to know that this intermediate
form exists before we delve further into the code generation process next, in
[The Compiler Backend Byte Code And Native Code](compiler-backend.html#the-compiler-backend-byte-code-and-native-code){data-type=xref}.

There are several new integrated tools emerging that combine these typed AST
files with common editors such as Emacs or Vim. The best of these is
[Merlin](https://github.com/def-lkb/merlin), which adds value and module
autocompletion, displays inferred types and can build and display errors
directly from within your editor. There are instructions available on its
homepage for configuring Merlin with your favorite editor.




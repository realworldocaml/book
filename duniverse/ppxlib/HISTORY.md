# History

This repository was originally a merge of several ppx-related
repositories, namely:

- [ppx_ast](https://github.com/janestreet/ppx_ast);
- [ppx_core](https://github.com/janestreet/ppx_core);
- [ppx_driver](https://github.com/janestreet/ppx_driver);
- [ppx_metaquot](https://github.com/janestreet/ppx_metaquot);
- [ppx_traverse](https://github.com/janestreet/ppx_traverse);
- [ppx_traverse_builtins](https://github.com/janestreet/ppx_traverse_builtins);
- [ppx_type_conv](https://github.com/janestreet/ppx_type_conv).

Since this merge, development has been happening only in this
repository and the aforementioned ones only contain synonym
definitions to provide backward compatibility.  The structure of this
repository is as follows:

- `ast/` contains the `ppxlib.ast` library, that replaces `ppx_ast`;
- `src/` contains the `ppxlib` library, that replaces `ppx_core`,
  `ppx_driver`, and `type_conv`;
- `metaquot/` contains the `ppxlib.metaquot` library, that replaces
  `ppx_metaquot`;
- `metaquot_lifters/` contains the `ppxlib.metaquot_lifters` library, that
  replaces `ppx_metaquot.lifters`;
- `print-diff/` contains the `ppxlib.print_diff` library, that replaces
  `ppx_driver.print_diff`;
- `runner/` contains the `ppxlib.runner` library, that replaces
  `ppx_driver.runner`;
- `runner_as_ppx/` contains the `ppxlib.runner_as_ppx` library, that replaces
  `ppx_driver.runner_as_ppx`;
- `traverse/` contains the `ppxlib.traverse` library, that replaces
  `ppx_traverse`;
- `traverse_builtins/` contains the `ppxlib.traverse_builtins/` library, that
  replaces `ppx_traverse_builtins`.


Ast
===
`Ppxlib_ast` selects a specific version of the OCaml Abstract Syntax Tree
from the [ocaml-migrate-parsetree](https://github.com/ocaml-ppx/ocaml-migrate-parsetree)
project that is not necessarily the same one as the one being used by the
compiler.

It also snapshots the corresponding parser and pretty-printer from the OCaml
compiler, to create a full frontend independent of the version of OCaml.

This AST is used in all Jane Street ppx rewriters, and more generally in all
ppx rewriters based on Ppxlib. Using a different AST allows to "detach"
the ppx code from the compiler libraries, and allow to use ppx rewriters with
new compilers before upgrading the ppx code.


Ppxlib
======
Ppxlib is a standard library for OCaml AST transformers, that uses the
AST from `Ppxlib_ast`. It features:

- various auto-generated AST traversal using an open recursion scheme
- helpers for building AST fragments
- helpers for matching AST fragments
- a framework for dealing with attributes and extension points
- spellchecking and other hints on misspelled/misplaced attributes and
  extension points
- checks for unused attributes (they are otherwise silently dropped by
  the compiler)

Other ASTs
----------
If you want to write code that works with several versions of
`Ppxlib` using different AST versions, you can use the versionned
alternatives for `Ast_builder` and `Ast_pattern`. For instance:

```
open Ppxlib
module Ast_builder = Ast_builder_403
module Ast_pattern = Ast_pattern_403
```

Drivers
-------
A driver is an executable created from a set of OCaml AST transformers linked
together with a command line frontend.

The aim is to provide a tool that can be used to:

- easily view the pre-processed version of a file, no need to construct a
  complex command line: `ppx file.ml` will do;
- use a single executable to run several transformations: no need to fork many
  times just for pre-processing;
- improved errors for misspelled/misplaced attributes and extension points.

### Using driver-based rewriters
The recommended way to use rewriters based on `Ppxlib.Driver` is through
[dune](https://github.com/ocaml/dune). All you need to is add this line to your
`(library ...)` or `(executables ...)` stanza:

```
(preprocess (pps (rewriter1 rewriter2 ... ppxlib.runner)))
```

dune will automatically build a static driver including all these rewriters.
Note the `ppxlib.runner` at the end of the list, it will still work if you
don't put but some specific features of `ppxlib` won't be available.

If you are not using dune, you can build a custom driver yourself using
ocamlfind.

These methods are described in the following sections.

### Creating a new Ppx\_driver based rewriter
If using dune, you can just use the following jbuild file:

```
(library
 ((name        my_ppx)
  (public_name my_ppx)
  (kind ppx_rewriter)
  (libraries (ppxlib))
  (ppx_runtime_libraries (<runtime dependencies if any>))
  (preprocess (pps (ppx_metaquot)))))
```

`(kind ppx_driver)` has two effects:

1. it links the library with `-linkall`. Since plugins register themselves with
   the Ppx\_driver library by doing a toplevel side effect, you need to be sure
   they are linked in the static driver to be taken into accound;
2. it instructs dune to produce a special META file that is compatible with the
   various ways of using ppx rewriters, i.e. for people not using dune.

### Building a custom driver using ocamlfind
To build a custom driver using ocamlfind, simply link all the AST transformers
together with the `ppxlib.runner` package at the end:

    ocamlfind ocamlopt -predicates ppx_driver -o ppx -linkpkg \
      -package ppx_sexp_conv -package ppx_bin_prot \
      -package ppxlib.runner

Normally, `ppxlib.driver`-based rewriters should be build with the
approriate `-linkall` option on individual libraries. If one is missing this
option, the code rewriter might not get linked in. If this is the case, a
workaround is to pass `-linkall` when linking the custom driver.


### Building rewriter that you are currently developing
Note: if using dune, you do not need to read this as dune already does all the
right things for you. This section is written having ocamlbuild in mind.

When developing a new rewriter you are very likely to prepare a few
tests for it. The compilation line above doesn't suit this task very well
(because ocamlfind package with your rewriter is not yet installed) 
and it will be more convenient to specify `.cmx[a]` with your rewriter
manually.

For example, let's suppose that the standalone rewriter (`pp_foo.native`) 
have this code

    let () = Ppxlib.Driver.standalone ()

in `pp_foo.ml` and your generator is begin loaded in `ppx_foo.ml`.
You need a few extra switches to compile standalone rewriter
(N.B. order matters)

    ocamlfind ... dependecy1_of_ppx_foo.cmx ... ppx_foo.cmx -package ppxlib -o pp_foo.native
    
or, if you have already created `ppx_foo.cmxa` using `-linkall` option 

    ocamlfind ... ppx_foo.cmxa -package ppxlib -o pp_foo.native

And now you can specify that your test suite uses your rewriter and
depends on a few extra `.cma`'s by adding a few lines into your `_tags` file

    <regression/test*.*>: ppx(./pp_foo.native --as-ppx)
    <regression/test*.*>: depends_on_foo
    
and specifying dependencies in your `myocamlbuild.ml` file using

    dep ["compile";"depends_on_foo"] ["ppx_foo.cmxa"; "pp_foo.native"]

### The driver as a command line tool
It recognizes the following command-line switches:

```
-loc-filename <string>      File name to use in locations
-reserve-namespace <string> Mark the given namespace as reserved
-no-check                   Disable checks (unsafe)
-apply <names>              Apply these transformations in order (comma-separated list)
-dont-apply <names>         Exclude these transformations
-no-merge                   Do not merge context free transformations (better for debugging rewriters)
-as-ppx                     Run as a -ppx rewriter (must be the first argument)
--as-ppx                    Same as -as-ppx
-as-pp                      Shorthand for: -dump-ast -embed-errors
--as-pp                     Same as -as-pp
-o <filename>               Output file (use '-' for stdout)
-                           Read input from stdin
-dump-ast                   Dump the marshaled ast to the output file instead of pretty-printing it
--dump-ast                  Same as -dump-ast
-dparsetree                 Print the parsetree (same as ocamlc -dparsetree)
-embed-errors               Embed errors in the output AST (default: true when -dump-ast, false otherwise)
-null                       Produce no output, except for errors
-impl <file>                Treat the input as a .ml file
--impl <file>               Same as -impl
-intf <file>                Treat the input as a .mli file
--intf <file>               Same as -intf
-debug-attribute-drop       Debug attribute dropping
-print-transformations      Print linked-in code transformations, in the order they are applied
-print-passes               Print the actual passes over the whole AST in the order they are applied
-ite-check                  No effect (kept for compatibility)
-pp <command>               Pipe sources through preprocessor <command> (incompatible with -as-ppx)
-reconcile                  (WIP) Pretty print the output using a mix of the input source and the generated code
-reconcile-with-comments    (WIP) same as -reconcile but uses comments to enclose the generated code
-no-color                   Don't use colors when printing errors
-diff-cmd                   Diff command when using code expectations
-pretty                     Instruct code generators to improve the prettiness of the generated code
-styler                     Code styler
-help                       Display this list of options
--help                      Display this list of options
```

When passed a file as argument, a ppx driver will pretty-print the code
transformed by all its built-in AST transformers. This gives a convenient way
of seeing the code generated for a given attribute/extension.

A driver can simply be used as the argument of the `-pp` option of the OCaml
compiler, or as the argument of the `-ppx` option by passing `-as-ppx` as first
argument:

```
$ ocamlc -c -pp "ppx -as-pp" file.ml
$ ocamlc -c -ppx "ppx -as-ppx" file.ml
```

### Rewriters as findlib libraries
Note: if using dune, you do not need to read this as dune already does all the
right things for you.

In normal operation, Ppxlib.Driver rewriters are packaged as findlib
libraries. When using dune everything is simple as preprocessors and normal
dependencies are separated. However historically, people have been specifying
both preprocessors and normal library dependencies together. Even worse, many
build system still don't use a static driver and call out to multiple ppx
commands to preprocess a single file, which slow downs compilation a lot.

In order for all these different methods to work properly, you need a peculiar
META file. The rules are explained below.

It is recommended to split the findlib package into two:

1. one for the main library, which almost assume it is just a normal library;
2. another sub-package one for:
   - allowing to mix preprocessors and normal dependencies;
   - the method of calling one executable per rewriter.

In the rest we'll assume we are writing a META file for a `ppx_foo` rewriter,
that itself uses the `ppxlib` and `re` libraries, and produces code using
`ppx_foo.runtime-lib`.

We want the META file to support all of these:

1. mix normal dependencies and preprocessors, using one executable per
   rewriter:

   ```
   ocamlfind ocamlc -package ppx_foo -c toto.ml
   ```
2. mix normal dependencies and preprocessors, using a single ppx driver:

   ```
   $ ocamlfind ocamlc -package ppx_foo -predicates custom_ppx \
      -ppx ./custom-driver.exe -c toto.ml
   ```
3. build a custom driver:

   ```
   $ ocamlfind ocamlc -linkpkg -package ppx_foo -predicates ppx_driver \
      -o custom-driver.exe
   ```
4. build systems properly specifying preprocessors as such, separated from
   normal dependencies, as dune does

Since preprocessors and normal dependencies are always specified separately in
jbuild files, dune just always set the `ppx_driver` predicates.

In the end the META file should look like this:

```
# Standard package, expect it assumes that the "ppx_driver" predicate is set
version                     = "42.0"
description                 = "interprets [%foo ...] extensions"
requires(ppx_driver)        = "ppxlib re"
archives(ppx_driver,byte)   = "ppx_foo.cma"
archives(ppx_driver,native) = "ppx_foo.cmxa"
plugin(ppx_driver,byte)     = "ppx_foo.cma"
plugin(ppx_driver,native)   = "ppx_foo.cmxs"

# This is what dune uses to find out the runtime dependencies of
# a preprocessor
ppx_runtime_deps = "ppx_foo.runtime-lib"

# This line makes things transparent for people mixing preprocessors
# and normal dependencies
requires(-ppx_driver) = "ppx_foo.deprecated-ppx-method"

package "deprecated-ppx-method" (
  description = "glue package for the deprecated method of using ppx"
  requires    = "ppx_foo.runtime-lib"
  ppx(-ppx_driver,-custom_ppx) = "./as-ppx.exe"
)

package "runtime-lib" ( ... )
```

You can check that this META works for all the 4 methods described
above.

Derivers
--------
The `Ppxlib.Deriving` module factors out functionality needed by
different preprocessors that generate code from type specifications.  Example
libraries currently depending on `Deriving`:

- `ppx_bin_prot`;
- `ppx_compare`;
- `ppx_fields_conv`;
- `ppx_sexp_conv`;
- `ppx_variants_conv`.

### Derivers compatibility with [`ppx_import`](https://github.com/ocaml-ppx/ppx_import)

`ppx_import` is a ppx rewriter that let's you import external type definitions. It will turn
```ocaml
type t = [%import A.t]
```
into:
```ocaml
type t = A.t = <actual A.t definition>
```
It spares you the need to copy the type definition and to update it when `A.t` definition changes.

`ppx_import` is thus often used in combination with ppx derivers.

Because `ppx_import` requires extra information from the compiler that aren't available when it is
initially called with `ocamldep`, it will not completely expand the type definition and instead
rewrite it as:
```ocaml
type t = A.t
```

That means that if you want your deriver to work with `ppx_import` and to be able to expand the
copied type definition, it must not fail during this intermediate stage.
If your deriver doesn't natively handle abstract type definitions, you can always return an empty
`structure_item` or `signature_item` list.

Compatibility with [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving)
----------------------------------------------------------------------------
`Ppxlib.Deriving`-based code generators are meant to be used with
`Ppxlib.Driver`. However `Deriving` allows to export a compatible
`ppx_deriving` plugin. By default, when not linked as part of a driver,
packages using `Deriving` will just use `ppx_deriving`.

So for instance this will work as expected using `ppx_deriving`:

    ocamlfind ocamlc -c -package ppx_sexp_conv foo.ml

For end users, the main advantage of using `Deriving`-based generators is that
it will catch typos and attributes misplacement. For instance:

```
# type t = int [@@derivin sexp]
Error: Attribute `derivin' was not used
Hint: Did you mean deriving?
# type t = int [@@deriving sxp]
Error: ppxlib_deriving: 'sxp' is not a supported type deriving generator
Hint: Did you mean sexp?
# type t = int [@deriving sexp]
Error: Attribute `deriving' was not used
Hint: `deriving' is available for type declarations, type extensions
and extension constructors but is used here in the context of a core type.
Did you put it at the wrong level?"
```

Deriving Syntax
---------------
This section is only relevant if you are not using `ppx_deriving`.

`Deriving` interprets the `[@@deriving ...]` attributes on type declarations,
exception declarations and extension constructor declarations:

```
type t = A | B [@@deriving sexp, bin_io]
```

`sexp` and `bin_io` are called generators. They are functions that generate
code given the declaration. These functions are implemented by external
libraries such as `ppx_sexp_conv` or `ppx_bin_prot`. `Deriving` itself
provides no generator, it does only the dispatch.

Generators can take arguments. This is done using the following syntax:

```
type t = A | B [@@deriving foo ~arg:42]
```

For arguments that are just switches, it is common to use the
following syntax:

```
type t = A | B [@@deriving foo ~bar]
```


Metaquot (and Metaquot_lifters)
===============================
`Ppxlib_metaquot` is a ppx rewriter allowing you to write values representing
the OCaml AST in the OCaml syntax.

For instance:

```
[%expr x + 1]
```

is a value of type `Ppxlib_ast.Ast.expression`, representing the OCaml
expression `x + 1`.

`Ppxlib_metaquot` is similar to [ppx_tools.metaquot](https://github.com/ocaml-ppx/ppx_tools),
expect that:

- it uses the version of the OCaml AST defined by Ppxlib_ast rather than the
  one from the current compiler
- it can be used simultaneously with other rewriters using
  `Ppxlib.Driver`.

`Ppxlib_metaquot_lifters` provides lifting functions for OCaml predefined
types (`int`, `string`, `list`, ...).


Traverse (and Traverse_builtins)
================================
`Ppxlib_traverse` is a `Deriving` plugin generating open recursion classes
from type definition. Users can overwrite a specific method of the generated
classes in order to specialize the recursion on specific nodes.
`Ppxlib_traverse` is in particular used to generate the open recursion classes
to traverse the OCaml AST.

For instance, this is the kind of code generated (the generated code is
between the `[@@deriving_inline ...]` and `[@@@end]`):

```
type expression =
  | Var   of string
  | Const of int
  | Add   of expression * expression
  | If    of cond * expression * expression

and cond =
  | Cond_var   of string
  | Cond_const of bool
  | Cond_and   of cond * cond

[@@deriving_inline traverse_map]
class map = object(self)
  method virtual int    : int    -> int
  method virtual string : string -> string
  method virtual int    : int    -> int

  method expression = function
    | Var x -> Var (self#string x)
    | Const x -> Const (self#int x)
    | Add (x, y) -> Add (self#expression x, self#expression y)
    | If (x, y, z) -> If (self#cond x, self#expression y, self#expression z)

  method cond = function
    | Cond_var x -> Cond_var (self#string x)
    | Cond_const x -> Cond_const (self#bool x)
    | Cond_and (x, y) -> Cond_and (self#cond x, self#cond y)
[@@end]
```

Now if you wanted to do a deep-copy of an expression, replacing boolean
variable `foo` by `true`:

```
let replace_var = object
  inherit Ppx_traverse_builtins.map
  inherit map as super

  method cond = function
    | Cond_var "foo" -> Cond_const true
    | c -> super#cond c
end

let replace_var expr = replace_var replace_var#expression expr
```

`Ppx_traverse_builtins.map` contains the definition for all the builtin types,
such as `int`, `string`, `list`, ...

Classes
-------

`Ppx_traverse` can generate the following classes: `map`, `iter`, `fold`,
`fold_map`, `map_with_context`, `lift`. `[@@deriving traverse]` is an alias
to generate all the supported classes.

`lift` is a special class that is mostly useful to lift an OCaml constant to
the AST that represent this constant. To do so, you can use
`Ppx_metaquot_lifters`:

```
type t = { x : int; y : int } [@@deriving traverse_lift]

let expression_of_t ~loc t : Ast.expression =
  let lift = object
    inherit Ppx_metaquot_lifters.expression_lifters loc
    inherit lift
  end in
  lift#t t
```
